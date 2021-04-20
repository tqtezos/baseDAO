-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
--
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- For all the incomplete list pattern matches in the calls to with
-- withOriginated func
module Test.Ligo.RegistryDAO
  ( test_RegistryDAO
  ) where

import Universum

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Test.Tasty (TestTree, testGroup)
import Time (sec)

import Lorentz as L
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Test.Consumer
import Michelson.Text (mkMTextUnsafe)
import Michelson.Typed (convertContract)
import Michelson.Typed.Convert (untypeValue)
import Michelson.Untyped.Entrypoints (unsafeBuildEpName)
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps, nettestScenarioOnEmulatorCaps)
import Util.Named

import qualified Ligo.BaseDAO.Common.Types as DAO
import Ligo.BaseDAO.Contract
import Ligo.BaseDAO.Types
import Ligo.Util
import Test.Ligo.BaseDAO.Common
  (addressToKeyHash, checkTokenBalance, dummyFA2Contract, makeProposalKey, sendXtz, totalSupplyFromLedger)

withOriginated
  :: MonadNettest caps base m
  => Integer
  -> ([Address] -> FullStorage)
  -> ([Address] -> FullStorage -> TAddress Parameter -> TAddress FA2.Parameter -> m a)
  -> m a
withOriginated addrCount storageFn tests = do
  addresses <- mapM (\x -> newAddress $ "address" <> (show x)) [1 ..addrCount]
  tokenContract <- originateSimple "token_contract" [] dummyFA2Contract
  let storageInitial = storageFn addresses
  let storage = storageInitial
        { fsStorage = (fsStorage storageInitial) { sGovernanceToken = GovernanceToken
            { gtAddress = unTAddress tokenContract
            , gtTokenId = FA2.theTokenId
            }
        } }

  baseDao <- originateLargeUntyped $ UntypedOriginateData
    { uodName = "BaseDAO - RegistryDAO Test Contract"
    , uodBalance = zeroMutez
    , uodStorage = untypeValue $ toVal storage
    , uodContract = convertContract baseDAOContractLigo
    }
  tests addresses storage (TAddress baseDao) tokenContract

-- | We test non-token entrypoints of the BaseDAO contract here
test_RegistryDAO :: [TestTree]
test_RegistryDAO =
  [ testGroup "RegistryDAO Tests"
    [ nettestScenarioOnEmulatorCaps "Calling the propose endpoint with an empty proposal works" $
        withOriginated 2
          (\(admin: wallet1:_) -> initialStorageWithExplictRegistryDAOConfig admin [wallet1]) $
          \(_:wallet1:_) _ baseDao _ -> do
            let proposalMeta = DynamicRec mempty
            let proposalSize = metadataSize proposalMeta
            withSender (AddressResolved wallet1) $
              call baseDao (Call @"Freeze") (#amount .! proposalSize, #keyhash .! (addressToKeyHash wallet1))

            -- Advance one voting period, which is currently set as 10 secs.
            advanceTime (sec $ 11)

            withSender (AddressResolved wallet1) $ call baseDao (Call @"Propose")
              (ProposeParams proposalSize proposalMeta)

    , nettestScenarioCaps "proposal exceeding max_proposal_size result in error" $
        withOriginated 2
          (\(admin: wallet1:_) -> initialStorageWithExplictRegistryDAOConfig admin [wallet1]) $
          \(_:wallet1:_) _ baseDao _ -> let
            -- In the explicitly set configuration max_proposal_size is set at 100.
            -- And here we create a proposal that is bigger then 100.
            proposalMeta = DynamicRec $ Map.fromList $
              [(mkMTextUnsafe ("long_key" <> (show @_ @Int t)), "long_value") | t <- [1..10]]
            proposalSize = metadataSize proposalMeta
            in withSender (AddressResolved wallet1) $ call
               baseDao (Call @"Propose") (ProposeParams proposalSize proposalMeta)
               & expectFailProposalCheck baseDao

    , nettestScenarioCaps "checks it fails if required tokens are not frozen" $
        withOriginated 2
          (\(admin: wallet1:_) -> initialStorageWithExplictRegistryDAOConfig admin [wallet1]) $
          \(_:wallet1:_) _ baseDao _ -> let
            proposalMeta = DynamicRec mempty
            -- Here we only freeze 2 tokens, but the proposal size and the configuration params
            -- frozen_scale_value, frozen_extra_value set to 1 and 0 means that it requires 6
            -- tokens to be frozen (6 * 1 + 0) because proposal size happen to be 6 here.
            in withSender (AddressResolved wallet1) $
               call baseDao (Call @"Propose") (ProposeParams 2 proposalMeta)
               & expectFailProposalCheck baseDao

    , nettestScenarioOnEmulatorCaps "check it correctly calculates required frozen tokens" $
        withOriginated 2
          (\(admin: wallet1:_) -> setExtra @Natural [mt|frozen_extra_value|] 2 $ initialStorageWithExplictRegistryDAOConfig admin [wallet1]) $
          \(_:wallet1:_) _ baseDao _ -> do
            withSender (AddressResolved wallet1) $
              call baseDao (Call @"Freeze") (#amount .! 8, #keyhash .! (addressToKeyHash wallet1))
            advanceTime (sec $ 11)
            let
              proposalMeta = DynamicRec mempty
            -- Here the proposal size and the configuration params frozen_scale_value,
            -- frozen_extra_value set to 1 and 2 means that it requires 8 tokens to be
            -- frozen (6 * 1 + 2) because proposal size happen to be 6 here.
            withSender (AddressResolved wallet1) $
               call baseDao (Call @"Propose") (ProposeParams 8 proposalMeta)

    , nettestScenarioOnEmulatorCaps "checks it correctly calculates tokens to burn when rejecting" $ do
        let frozen_scale_value = 2
        let frozen_extra_value = 0
        let slash_scale_value = 1
        let slash_division_value = 4
        withOriginated 2
          (\(admin: wallet1:_) ->
            setExtra @Natural [mt|frozen_scale_value|] frozen_scale_value $
            setExtra @Natural [mt|frozen_extra_value|] frozen_extra_value $
            setExtra @Natural [mt|slash_scale_value|] slash_scale_value $
            setExtra @Natural [mt|slash_division_value|] slash_division_value $ initialStorageWithExplictRegistryDAOConfig admin [wallet1]) $

          \(admin: wallet1: _) _ baseDao _ -> let
            proposalMeta1 = DynamicRec $ Map.fromList $ [([mt|key1|], "val"), ([mt|key2|], "val")] -- 44
            proposalSize1 = metadataSize proposalMeta1

            in do
              advanceTime (sec 10) -- voting period is 10 secs
              let requiredFrozen = proposalSize1 * frozen_scale_value + frozen_extra_value

              withSender (AddressResolved wallet1) $
                call baseDao (Call @"Freeze") (#amount .! requiredFrozen, #keyhash .! (addressToKeyHash wallet1))

              advanceTime (sec 10) -- voting period is 10 secs

              withSender (AddressResolved wallet1) $
                call baseDao (Call @"Propose") (ProposeParams requiredFrozen proposalMeta1)

              advanceTime (sec 15) -- voting period is 10 secs
              withSender (AddressResolved admin) $
                call baseDao (Call @"Flush") (1 :: Natural)

              -- Since we have frozen_scale_value = 2, slash_scale_value = 1 and slash_division_value = 2
              -- After the rejection above, we expect that (2 * proposalSize1/2) tokens
              -- will be returned to the proposer.  So they will have a balance
              -- of 100 - proposalSize1.

              -- Check the balance of wallet1 using 'Balance_of' entrypoint.
              consumer <- originateSimple "consumer" [] (contractConsumer @[FA2.BalanceResponseItem])
              let balanceRequestItem = FA2.BalanceRequestItem { briOwner = wallet1, briTokenId = frozenTokenId }
              let balanceRequest = FA2.mkFA2View [balanceRequestItem] consumer
              withSender (AddressResolved wallet1) $ call baseDao (Call @"Balance_of") balanceRequest

              let spent = div (requiredFrozen * slash_scale_value) slash_division_value

              checkStorage (AddressResolved $ unTAddress consumer) (toVal [[FA2.BalanceResponseItem balanceRequestItem (defaultTokenBalance + (requiredFrozen - spent))]])

    , nettestScenarioOnEmulatorCaps "checks it correctly executes the proposal that has won" $ do
        let frozen_scale_value = 1
        let frozen_extra_value = 0
        let slash_scale_value = 1
        let slash_division_value = 1
        withOriginated 3
          (\(admin: wallet1: voter1:_) ->
            setExtra @Natural [mt|frozen_scale_value|] frozen_scale_value $
            setExtra @Natural [mt|frozen_extra_value|] frozen_extra_value $
            setExtra @Natural [mt|slash_scale_value|] slash_scale_value $
            setExtra @Natural [mt|max_proposal_size|] 200 $
            setExtra @Natural [mt|slash_division_value|] slash_division_value $ initialStorageWithExplictRegistryDAOConfig admin [wallet1, voter1]) $

          \(admin: wallet1: voter1 : _) _ baseDao _ -> let
            -- We currently have max_proposal_size of 200, but the following proposal is 317 bytes long.
            largeProposalMeta = DynamicRec $ Map.fromList $ [(mkMTextUnsafe ("long_key" <> (show @_ @Int t)), "long_value") | t <- [1..10]]
            largeProposalSize = metadataSize largeProposalMeta

            in do
              withSender (AddressResolved admin) $
                call baseDao (Call @"Set_quorum_threshold") $ QuorumThreshold 1 100

              advanceTime (sec 10) -- voting period is 10 secs
              let requiredFrozen = largeProposalSize * frozen_scale_value + frozen_extra_value

              withSender (AddressResolved wallet1) $
                call baseDao (Call @"Freeze") (#amount .! 400, #keyhash .! (addressToKeyHash wallet1))

              withSender (AddressResolved voter1) $
                call baseDao (Call @"Freeze") (#amount .! 100, #keyhash .! (addressToKeyHash voter1))

              advanceTime (sec 11) -- voting period is 10 secs

              -- We expect this to fail because max_proposal_size is 200 and proposal size is 317.
              withSender (AddressResolved wallet1) $
                call baseDao (Call @"Propose") (ProposeParams requiredFrozen largeProposalMeta)
                & expectFailProposalCheck baseDao

              -- We create a new proposal to increase max_proposal_size to largeProposalSize + 1.
              let sMaxUpdateproposalMeta1 = DynamicRec $ Map.fromList
                    [([mt|max_proposal_size|], lPackValueRaw $ Just ((largeProposalSize + 1) :: Natural))
                    ,([mt|frozen_scale_value|], lPackValueRaw @(Maybe Natural) Nothing)
                    ,([mt|frozen_extra_value|], lPackValueRaw @(Maybe Natural) Nothing)
                    ,([mt|slash_scale_value|], lPackValueRaw @(Maybe Natural) Nothing)
                    ,([mt|slash_division_value|], lPackValueRaw @(Maybe Natural) Nothing)
                    ]
              let sMaxUpdateproposalSize1 = metadataSize sMaxUpdateproposalMeta1
              let requiredFrozenForUpdate = sMaxUpdateproposalSize1 * frozen_scale_value + frozen_extra_value

              withSender (AddressResolved wallet1) $
                call baseDao (Call @"Propose") (ProposeParams requiredFrozenForUpdate sMaxUpdateproposalMeta1)

              advanceTime (sec 10) -- voting period is 10 secs
              -- Then we send 60 upvotes for the proposal (as min quorum is 1% of 500)
              let proposalKey = makeProposalKey (ProposeParams requiredFrozenForUpdate sMaxUpdateproposalMeta1) wallet1
              withSender (AddressResolved voter1) $
                call baseDao (Call @"Vote") [PermitProtected (VoteParam proposalKey True 60) Nothing]

              advanceTime (sec 11)
              withSender (AddressResolved admin) $
                call baseDao (Call @"Flush") (1 :: Natural)

              -- Now we expect this to work
              withSender (AddressResolved wallet1) $
                call baseDao (Call @"Propose") (ProposeParams requiredFrozen largeProposalMeta)

    , nettestScenarioOnEmulatorCaps "checks on-chain view correctly returns the registry value" $ do
        -- The default values assigned from initialStorageWithExplictRegistryDAOConfig function
        withOriginated 3
          (\(admin: wallet1: voter1:_) -> setExtra @Natural [mt|max_proposal_size|] 200 $
              initialStorageWithExplictRegistryDAOConfig admin [wallet1, voter1]) $

          \(admin: wallet1: voter1 : _) _fs baseDao _ -> do
            let
              proposalMeta = DynamicRec $ Map.fromList $
                [ ([mt|updates|], lPackValueRaw [([mt|key|], Just [mt|testVal|])])
                , ([mt|agoraPostID|], lPackValueRaw @Natural 10)
                ]
              proposalSize = metadataSize proposalMeta

            withSender (AddressResolved admin) $
              call baseDao (Call @"Set_quorum_threshold") $ QuorumThreshold 1 100

            withSender (AddressResolved wallet1) $
              call baseDao (Call @"Freeze") (#amount .! proposalSize, #keyhash .! (addressToKeyHash wallet1))

            withSender (AddressResolved voter1) $
              call baseDao (Call @"Freeze") (#amount .! 50, #keyhash .! (addressToKeyHash voter1))
            advanceTime (sec 13) -- voting period is 10 secs

            let requiredFrozen = proposalSize -- since frozen_scale_value and frozen_scale_value are 1 and 0.

            -- We propose the addition of a new registry key
            withSender (AddressResolved wallet1) $
              call baseDao (Call @"Propose") (ProposeParams requiredFrozen proposalMeta)

            advanceTime (sec 12)
            -- Then we send 50 upvotes for the proposal (as min quorum is 1% of total frozen tokens)
            let proposalKey = makeProposalKey (ProposeParams requiredFrozen proposalMeta) wallet1
            withSender (AddressResolved voter1) $
              call baseDao (Call @"Vote") [PermitProtected (VoteParam proposalKey True 50) Nothing]

            advanceTime (sec 12)
            withSender (AddressResolved admin) $
              call baseDao (Call @"Flush") (1 :: Natural)

            consumer <- originateSimple "consumer" [] (contractConsumer @(MText, (Maybe MText)))

            withSender (AddressResolved voter1) $
              call baseDao (Call @"CallCustom") ([mt|lookup_registry|], lPackValueRaw ([mt|key|], consumer))

            checkStorage (AddressResolved $ unTAddress consumer) (toVal [([mt|key|], Just [mt|testVal|])])

    , nettestScenarioOnEmulatorCaps "checks it can flush a transfer type proposal (#66)" $
        withOriginated 3
          (\(admin : wallets) ->
            setExtra @Natural [mt|max_proposal_size|] 200 $
            initialStorageWithExplictRegistryDAOConfig admin wallets) $
          \[admin, wallet1, wallet2] _ baseDao tokenContract -> do

            let
              proposalMeta = DynamicRec $ Map.fromList $
                [ ([mt|agoraPostID|], lPackValueRaw @Natural 1)
                , ([mt|transfers|], lPackValueRaw @([DAO.TransferType])
                    [ tokenTransferType (toAddress tokenContract) wallet2 wallet1
                    ]
                  )
                ]
              proposalSize = metadataSize proposalMeta
              proposeParams = ProposeParams proposalSize proposalMeta

            withSender (AddressResolved wallet1) $
              call baseDao (Call @"Freeze") (#amount .! proposalSize, #keyhash .! (addressToKeyHash wallet1))
            withSender (AddressResolved wallet2) $
              call baseDao (Call @"Freeze") (#amount .! 20, #keyhash .! (addressToKeyHash wallet2))

            advanceTime (sec 13)

            withSender (AddressResolved wallet1) $
              call baseDao (Call @"Propose") proposeParams

            checkTokenBalance frozenTokenId baseDao wallet1 164

            let
              key1 = makeProposalKey proposeParams wallet1
              upvote = NoPermit VoteParam
                  { vVoteType = True
                  , vVoteAmount = 20
                  , vProposalKey = key1
                  }

            advanceTime (sec 12)
            withSender (AddressResolved wallet2) $ call baseDao (Call @"Vote") [upvote]
            advanceTime (sec 11)
            withSender (AddressResolved admin) $ call baseDao (Call @"Flush") (1 :: Natural)

            checkTokenBalance frozenTokenId baseDao wallet1 (defaultTokenBalance + proposalSize)
            checkTokenBalance frozenTokenId baseDao wallet2 (defaultTokenBalance + 20)
            checkStorage (AddressResolved $ unTAddress tokenContract)
              (toVal
                [ [ FA2.TransferItem { tiFrom = wallet2, tiTxs = [FA2.TransferDestination { tdTo = wallet1 , tdTokenId = FA2.theTokenId, tdAmount = 10 }] } ] -- Actual transfer
                , [ FA2.TransferItem { tiFrom = wallet2, tiTxs = [FA2.TransferDestination { tdTo = unTAddress baseDao, tdTokenId = FA2.theTokenId, tdAmount = 20 }] } ] -- Wallet2 freezes 20 tokens
                , [ FA2.TransferItem { tiFrom = wallet1, tiTxs = [FA2.TransferDestination { tdTo = unTAddress baseDao, tdTokenId = FA2.theTokenId, tdAmount = proposalSize }] } ] -- governance token transfer for freeze
                ])

    , nettestScenarioOnEmulatorCaps "checks it can propose a valid xtz type proposal (#66)" $
        withOriginated 2
          (\(admin : wallets) ->
            setExtra @Natural [mt|max_proposal_size|] 200 $
            initialStorageWithExplictRegistryDAOConfig admin wallets) $
          \[_, wallet] _ baseDao _ -> do
            let
              proposalMeta = DynamicRec $ Map.fromList $
                [ ([mt|agoraPostID|], lPackValueRaw @Natural 1)
                , ([mt|transfers|], lPackValueRaw @([DAO.TransferType])
                    [ xtzTransferType 10 wallet
                    ]
                  )
                ]
              proposalMeta2 = DynamicRec $ Map.fromList $
                [ ([mt|agoraPostID|], lPackValueRaw @Natural 2)
                , ([mt|transfers|], lPackValueRaw @([DAO.TransferType])
                    [ xtzTransferType 3 wallet
                    ]
                  )
                ]
              proposalSize = metadataSize proposalMeta
              proposalSize2 = metadataSize proposalMeta2

            withSender (AddressResolved wallet) $
              call baseDao (Call @"Freeze") (#amount .! proposalSize, #keyhash .! (addressToKeyHash wallet))

            advanceTime (sec 10)

            -- Fails because 10 >= max_xtz_amount
            withSender (AddressResolved wallet) $
              call baseDao (Call @"Propose") (ProposeParams proposalSize proposalMeta)
              & expectFailProposalCheck baseDao

            withSender (AddressResolved wallet) $
              call baseDao (Call @"Freeze") (#amount .! proposalSize2, #keyhash .! (addressToKeyHash wallet))

            advanceTime (sec 10)

            withSender (AddressResolved wallet) $
              call baseDao (Call @"Propose") (ProposeParams proposalSize2 proposalMeta2)

            checkTokenBalance frozenTokenId baseDao wallet (defaultTokenBalance + 184)

    , nettestScenarioOnEmulatorCaps "checks it can transfer with receive_xtz_entrypoint (#66)" $
        withOriginated 3
          (\(admin : wallets) ->
            setExtra @Natural [mt|max_proposal_size|] 200 $
            initialStorageWithExplictRegistryDAOConfig admin wallets) $
          \[admin, wallet1, wallet2] _ baseDao _ -> do
            sendXtz (toAddress baseDao) (unsafeBuildEpName "callCustom") ([mt|receive_xtz|], lPackValueRaw ())

            let
              proposalMeta = DynamicRec $ Map.fromList $
                [ ([mt|agoraPostID|], lPackValueRaw @Natural 1)
                , ([mt|transfers|], lPackValueRaw @([DAO.TransferType])
                    -- transfer from baseDAO to wallet2
                    [ xtzTransferType 3 wallet2
                    ]
                  )
                ]
              proposalSize = metadataSize proposalMeta
              proposeParams = ProposeParams proposalSize proposalMeta

            withSender (AddressResolved wallet1) $
              call baseDao (Call @"Freeze") (#amount .! proposalSize, #keyhash .! (addressToKeyHash wallet1))
            withSender (AddressResolved wallet2) $
              call baseDao (Call @"Freeze") (#amount .! 10, #keyhash .! (addressToKeyHash wallet2))

            advanceTime (sec 11)

            withSender (AddressResolved wallet1) $
              call baseDao (Call @"Propose") proposeParams
            let key1 = makeProposalKey proposeParams wallet1

            checkTokenBalance frozenTokenId baseDao wallet1 (defaultTokenBalance + 92)

            let
              upvote = NoPermit VoteParam
                { vVoteType = True
                , vVoteAmount = 2
                , vProposalKey = key1
                }

            advanceTime (sec 11)
            withSender (AddressResolved wallet2) $ call baseDao (Call @"Vote") [upvote]
            advanceTime (sec 10)
            withSender (AddressResolved admin) $ call baseDao (Call @"Flush") (1 :: Natural)
    ]
  ]
  where

    defaultTokenBalance :: Natural = 0

    metadataSize :: DynamicRec "pm" -> Natural
    metadataSize md = fromIntegral $ BS.length $ lPackValueRaw md

    -- Here we parse the storage value from compiled ligo storage, which
    -- contains the RegistryDAO lambdas implemented in LIGO, and we just use
    -- `fromVal` to convert it to a 'FullStorage'. Then we can set the
    -- RegistryDAO configuration values using the setExtra function below, and
    -- initialize the contract using it. This let us have the lambdas from LIGO
    -- in storage, and allows to tweak RegistryDAO configuration in tests.
    initialStorage :: Address -> [Address] -> FullStorage
    initialStorage admin wallets = let
      fs = fromVal ($(fetchValue @FullStorage "haskell/test/registryDAO_storage.tz" "REGISTRY_STORAGE_PATH"))
      oldStorage = fsStorage fs

      ledger = BigMap $ Map.fromList [((w, frozenTokenId), defaultTokenBalance) | w <- wallets]

      newStorage = oldStorage
        { sAdmin = admin
        , sLedger = ledger
        , sTotalSupply = totalSupplyFromLedger ledger
        }
      in fs { fsStorage = newStorage }

    initialStorageWithExplictRegistryDAOConfig :: Address -> [Address] -> FullStorage
    initialStorageWithExplictRegistryDAOConfig admin wallets =
      setExtra @Natural [mt|frozen_scale_value|] 1 $
      setExtra @Natural [mt|frozen_extra_value|] 0 $
      setExtra @Natural [mt|slash_scale_value|] 1 $
      setExtra @Natural [mt|slash_division_value|] 1 $
      setExtra @Natural [mt|min_xtz_amount|] 2 $
      setExtra @Natural [mt|max_xtz_amount|] 5 $
      setExtra @Natural [mt|max_proposal_size|] 100 (initialStorage admin wallets)

    setExtra :: forall a. NicePackedValue a => MText -> a -> FullStorage -> FullStorage
    setExtra key v (s@FullStorage {..}) = let
     oldExtra = unDynamic $ sExtra fsStorage
     newExtra = Map.insert key (lPackValueRaw v) oldExtra
     newStorage = fsStorage { sExtra = DynamicRec newExtra }
     in s { fsStorage = newStorage }


expectFailProposalCheck
  :: (MonadNettest caps base m, ToAddress addr)
  => addr -> m a -> m ()
expectFailProposalCheck = expectCustomErrorNoArg #fAIL_PROPOSAL_CHECK

--------------------------------------------------------------------------
-- Helper
--------------------------------------------------------------------------

xtzTransferType :: Word32 -> Address -> DAO.TransferType
xtzTransferType amt toAddr = DAO.Xtz_transfer_type DAO.XtzTransfer
  { xtAmount = toMutez amt
  , xtRecipient = toAddr
  }

tokenTransferType :: Address -> Address -> Address -> DAO.TransferType
tokenTransferType contractAddr fromAddr toAddr = DAO.Token_transfer_type DAO.TokenTransfer
  { ttContractAddress = contractAddr
  , ttTransferList =
    [ FA2.TransferItem
      { tiFrom = fromAddr
      , tiTxs =
        [ FA2.TransferDestination
          { tdTo = toAddr
          , tdTokenId = FA2.theTokenId
          , tdAmount = 10
          }
        ]
      }
    ]
  }
