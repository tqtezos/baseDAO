-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
--
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-top-binds #-}
-- For all the incomplete list pattern matches in the calls to with
-- withOriginated func
module Test.Ligo.RegistryDAO
  ( test_RegistryDAO
  ) where

import Universum

import qualified Data.ByteString as BS
import Test.Tasty (TestTree, testGroup)

import Lorentz as L
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Test.Consumer
import Michelson.Text (unsafeMkMText)
import Michelson.Typed (convertContract)
import Michelson.Typed.Convert (untypeValue)
import Michelson.Untyped.Entrypoints (unsafeBuildEpName)
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps, nettestScenarioOnEmulatorCaps)
import Util.Named

import Ligo.BaseDAO.Common.Types
import Ligo.BaseDAO.Contract
import Ligo.BaseDAO.Types
import Ligo.Util
import Test.Ligo.BaseDAO.Common

-- | Helper type for unpack/pack
data RegistryDaoProposalMetadata
  = Update_receivers_proposal UpdateReceiverParam
  | Configuration_proposal ConfigProposal
  | Transfer_proposal TransferProposal
  | Update_guardian Address

instance HasAnnotation RegistryDaoProposalMetadata where
  annOptions = baseDaoAnnOptions

data UpdateReceiverParam
  = Add_receivers [Address]
  | Remove_receivers [Address]

instance HasAnnotation UpdateReceiverParam where
  annOptions = baseDaoAnnOptions


data TransferProposal = TransferProposal
  { tpAgoraPostId  :: Natural
  , tpTransfers    :: [TransferType]
  , tpRegistryDiff :: [(MText, Maybe MText)]
  }

instance HasAnnotation TransferProposal where
  annOptions = baseDaoAnnOptions


data ConfigProposal = ConfigProposal
  { cpFrozenScaleValue :: Maybe Natural
  , cpFrozenExtraValue :: Maybe Natural
  , cpSlashScaleValue :: Maybe Natural
  , cpSlashDivisionValue :: Maybe Natural
  , cpMaxProposalSize :: Maybe Natural
  }

instance HasAnnotation ConfigProposal where
  annOptions = baseDaoAnnOptions

customGeneric "RegistryDaoProposalMetadata" ligoLayout
deriving anyclass instance IsoValue RegistryDaoProposalMetadata

customGeneric "UpdateReceiverParam" ligoLayout
deriving anyclass instance IsoValue UpdateReceiverParam

customGeneric "ConfigProposal" ligoLayout
deriving anyclass instance IsoValue ConfigProposal

customGeneric "TransferProposal" ligoLayout
deriving anyclass instance IsoValue TransferProposal

withOriginated
  :: MonadNettest caps base m
  => Integer
  -> ([Address] -> FullStorage)
  -> ([Address] -> FullStorage -> TAddress Parameter -> TAddress FA2.Parameter -> m a)
  -> m a
withOriginated addrCount storageFn tests = do
  addresses <- mapM (\x -> newAddress $ fromString ("address" <> (show x))) [1 ..addrCount]
  dodTokenContract <- chAddress <$> originateSimple "token_contract" [] dummyFA2Contract
  let storageInitial = storageFn addresses
  now_level <- getLevel
  let storage = storageInitial
        { fsStorage = (fsStorage storageInitial)
          { sGovernanceToken = GovernanceToken
            { gtAddress = dodTokenContract
            , gtTokenId = FA2.theTokenId
            }
          , sStartLevel = now_level
          }
        }

  baseDao <- originateUntyped $ UntypedOriginateData
    { uodName = "BaseDAO - RegistryDAO Test Contract"
    , uodBalance = zeroMutez
    , uodStorage = untypeValue $ toVal storage
    , uodContract = convertContract baseDAOContractLigo
    }
  tests addresses storage (TAddress baseDao) (TAddress dodTokenContract)

toPeriod :: FullStorage -> Natural
toPeriod = unPeriod . cPeriod . fsConfig

-- | We test non-token entrypoints of the BaseDAO contract here
test_RegistryDAO :: [TestTree]
test_RegistryDAO =
  [ testGroup "RegistryDAO Tests"
    [ nettestScenarioOnEmulatorCaps "Calling the propose endpoint with an empty proposal works" $
        withOriginated 2
          (\(admin: _) -> initialStorageWithExplictRegistryDAOConfig admin) $
          \(_:wallet1:_) fs baseDao _ -> do
            let proposalMeta = lPackValueRaw @RegistryDaoProposalMetadata $
                  Transfer_proposal $ TransferProposal 1 [] []
            let proposalSize = metadataSize proposalMeta
            withSender wallet1 $
              call baseDao (Call @"Freeze") (#amount .! proposalSize)

            -- Advance one voting period to a proposing stage.
            advanceLevel (toPeriod fs)

            withSender wallet1 $ call baseDao (Call @"Propose")
              (ProposeParams wallet1 proposalSize proposalMeta)

    , nettestScenarioCaps "proposal exceeding max_proposal_size result in error" $
        withOriginated 2
          (\(admin: _) -> initialStorageWithExplictRegistryDAOConfig admin) $
          \(_:wallet1:_) _ baseDao _ -> let
            -- In the explicitly set configuration max_proposal_size is set at 100.
            -- And here we create a proposal that is bigger then 100.
            proposalMeta = lPackValueRaw @RegistryDaoProposalMetadata $
              Transfer_proposal $ TransferProposal 1 [] $
                [(unsafeMkMText ("long_key" <> (show @_ @Int t)), Just [mt|long_value|]) | t <- [1..10]]
            proposalSize = metadataSize proposalMeta
            in withSender wallet1 $ call
               baseDao (Call @"Propose") (ProposeParams wallet1 proposalSize proposalMeta)
                & expectFailProposalCheck tooLargeProposalErrMsg

    , nettestScenarioCaps "proposal_check: fail when xtz transfer contains 0 mutez" $
        withOriginated 2
          (\(admin: _) ->
              initialStorageWithExplictRegistryDAOConfig admin
                & setExtra @Natural [mt|min_xtz_amount|] 0
          ) $
          \(_:wallet1:_) fs baseDao _ -> do
            let proposalMeta = lPackValueRaw @RegistryDaoProposalMetadata $
                  Transfer_proposal $
                    TransferProposal 1 [ xtzTransferType 0 wallet1 ] []
            let proposalSize = metadataSize proposalMeta
            withSender wallet1 $
              call baseDao (Call @"Freeze") (#amount .! proposalSize)

            -- Advance one voting period to a proposing stage.
            advanceLevel (toPeriod fs)

            withSender wallet1 $ call baseDao (Call @"Propose")
              (ProposeParams wallet1 proposalSize proposalMeta)
                & expectFailProposalCheck zeroMutezErrMsg

    , nettestScenarioCaps "checks it fails if required tokens are not frozen" $
        withOriginated 2
          (\(admin: _) -> initialStorageWithExplictRegistryDAOConfig admin) $
          \(_:wallet1:_) _ baseDao _ -> let
            proposalMeta = lPackValueRaw @RegistryDaoProposalMetadata $
              Transfer_proposal $ TransferProposal 1 [] []
            -- Here we only freeze 2 tokens, but the proposal size and the configuration params
            -- frozen_scale_value, frozen_extra_value set to 1 and 0 means that it requires 6
            -- tokens to be frozen (6 * 1 + 0) because proposal size happen to be 6 here.
            in withSender wallet1 $
               call baseDao (Call @"Propose") (ProposeParams wallet1 2 proposalMeta)
               & expectFailProposalCheck incorrectTokenAmountErrMsg

    , nettestScenarioOnEmulatorCaps "check it correctly calculates required frozen tokens" $
        withOriginated 2
          (\(admin: _) -> setExtra @Natural [mt|frozen_extra_value|] 2 $ initialStorageWithExplictRegistryDAOConfig admin) $
          \(_:wallet1:_) fs baseDao _ -> do
            let
              proposalMeta = lPackValueRaw @RegistryDaoProposalMetadata $
                Transfer_proposal $ TransferProposal 1 [] []
              proposalSize = metadataSize proposalMeta -- 10

            withSender wallet1 $
              call baseDao (Call @"Freeze") (#amount .! (proposalSize + 2))
            advanceLevel (toPeriod fs)

            -- Here the proposal size and the configuration params frozen_scale_value,
            -- frozen_extra_value set to 1 and 2 means that it requires 12 tokens to be
            -- frozen (10 * 1 + 2) if proposal size is 10.
            withSender wallet1 $
               call baseDao (Call @"Propose") (ProposeParams wallet1 (proposalSize + 2) proposalMeta)

    , nettestScenarioOnEmulatorCaps "checks it correctly calculates tokens to burn when rejecting" $ do
        let frozen_scale_value = 2
        let frozen_extra_value = 0
        let slash_scale_value = 1
        let slash_division_value = 4
        withOriginated 2
          (\(admin: _) ->
            setExtra @Natural [mt|frozen_scale_value|] frozen_scale_value $
            setExtra @Natural [mt|frozen_extra_value|] frozen_extra_value $
            setExtra @Natural [mt|slash_scale_value|] slash_scale_value $
            setExtra @Natural [mt|slash_division_value|] slash_division_value $ initialStorageWithExplictRegistryDAOConfig admin) $

          \(admin: wallet1: _) (toPeriod -> period) baseDao _ -> let
            proposalMeta1 = lPackValueRaw @RegistryDaoProposalMetadata $
              Transfer_proposal $ TransferProposal 1 [] []
            proposalSize1 = metadataSize proposalMeta1

            in do
              let requiredFrozen = proposalSize1 * frozen_scale_value + frozen_extra_value

              withSender wallet1 $
                call baseDao (Call @"Freeze") (#amount .! requiredFrozen)

              -- Advance one voting period to a proposing stage.
              advanceLevel period

              withSender wallet1 $
                call baseDao (Call @"Propose") (ProposeParams wallet1 requiredFrozen proposalMeta1)

              -- Advance two voting periods to another proposing stage.
              advanceLevel (2 * period + 1) -- 22 is `proposal_flush_time`
              withSender admin $
                call baseDao (Call @"Flush") (1 :: Natural)

              -- Since we have frozen_scale_value = 2, slash_scale_value = 1 and slash_division_value = 2
              -- After the rejection above, we expect that (2 * proposalSize1/2) tokens
              -- will be returned to the proposer.  So they will have a balance
              -- of 100 - proposalSize1.



              let spent = div (requiredFrozen * slash_scale_value) slash_division_value

              checkBalanceEmulator (unTAddress baseDao) wallet1 (requiredFrozen - spent)


    , nettestScenarioOnEmulatorCaps "checks it correctly executes the proposal that has won" $ do
        let frozen_scale_value = 1
        let frozen_extra_value = 0
        let slash_scale_value = 1
        let slash_division_value = 1
        withOriginated 3
          (\(admin: _) ->
            setExtra @Natural [mt|frozen_scale_value|] frozen_scale_value $
            setExtra @Natural [mt|frozen_extra_value|] frozen_extra_value $
            setExtra @Natural [mt|slash_scale_value|] slash_scale_value $
            setExtra @Natural [mt|max_proposal_size|] 200 $
            setExtra @Natural [mt|slash_division_value|] slash_division_value $ initialStorageWithExplictRegistryDAOConfig admin) $

          \(admin: wallet1: voter1 : _) (toPeriod -> period) baseDao _ -> let
            -- We currently have max_proposal_size of 200, but the following proposal is 341 bytes long.
            largeProposalMeta = lPackValueRaw @RegistryDaoProposalMetadata $
              Transfer_proposal $ TransferProposal 1 [] $
                [(unsafeMkMText ("long_key" <> (show @_ @Int t)), Just [mt|long_value|]) | t <- [1..10]]
            largeProposalSize = metadataSize largeProposalMeta -- 341

            in do
              let requiredFrozen = largeProposalSize * frozen_scale_value + frozen_extra_value

              withSender wallet1 $
                call baseDao (Call @"Freeze") (#amount .! 400)

              withSender voter1 $
                call baseDao (Call @"Freeze") (#amount .! 100)

              -- Advance one voting period to a proposing stage.
              advanceLevel period

              -- We expect this to fail because max_proposal_size is 200 and proposal size is 341.
              withSender wallet1 $
                call baseDao (Call @"Propose") (ProposeParams wallet1 requiredFrozen largeProposalMeta)
                & expectFailProposalCheck tooLargeProposalErrMsg

              -- We create a new proposal to increase max_proposal_size to largeProposalSize + 1.
              let sMaxUpdateproposalMeta1 = lPackValueRaw @RegistryDaoProposalMetadata $
                    Configuration_proposal $ ConfigProposal
                        { cpFrozenScaleValue = Nothing
                        , cpFrozenExtraValue = Nothing
                        , cpSlashScaleValue = Nothing
                        , cpSlashDivisionValue = Nothing
                        , cpMaxProposalSize = Just $ largeProposalSize + 1
                        }
              let sMaxUpdateproposalSize1 = metadataSize sMaxUpdateproposalMeta1
              let requiredFrozenForUpdate = sMaxUpdateproposalSize1 * frozen_scale_value + frozen_extra_value

              withSender wallet1 $
                call baseDao (Call @"Propose") (ProposeParams wallet1 requiredFrozenForUpdate sMaxUpdateproposalMeta1)

              -- Advance one voting period to a voting stage.
              advanceLevel period
              -- Then we send 60 upvotes for the proposal (as min quorum is 1% of 500)
              let proposalKey = makeProposalKey (ProposeParams wallet1 requiredFrozenForUpdate sMaxUpdateproposalMeta1)
              withSender voter1 $
                call baseDao (Call @"Vote") [PermitProtected (VoteParam proposalKey True 60 voter1) Nothing]

              -- Advance one voting period to a proposing stage.
              advanceLevel (period + 1)
              withSender admin $
                call baseDao (Call @"Flush") (1 :: Natural)

              -- Now we expect this to work
              withSender wallet1 $
                call baseDao (Call @"Propose") (ProposeParams wallet1 requiredFrozen largeProposalMeta)

    , nettestScenarioOnEmulatorCaps "checks on-chain view correctly returns the registry value" $ do
        -- The default values assigned from initialStorageWithExplictRegistryDAOConfig function
        withOriginated 3
          (\(admin:_) -> setExtra @Natural [mt|max_proposal_size|] 200 $
              initialStorageWithExplictRegistryDAOConfig admin) $

          \(admin: wallet1: voter1 : _) (toPeriod -> period) baseDao _ -> do
            let
              proposalMeta = lPackValueRaw @RegistryDaoProposalMetadata $
                Transfer_proposal $ TransferProposal 1 [] [([mt|key|], Just [mt|testVal|])]

              proposalSize = metadataSize proposalMeta

            withSender wallet1 $
              call baseDao (Call @"Freeze") (#amount .! proposalSize)

            withSender voter1 $
              call baseDao (Call @"Freeze") (#amount .! 50)
            -- Advance one voting period to a proposing stage.
            advanceLevel period

            let requiredFrozen = proposalSize -- since frozen_scale_value and frozen_scale_value are 1 and 0.

            -- We propose the addition of a new registry key
            withSender wallet1 $
              call baseDao (Call @"Propose") (ProposeParams wallet1 requiredFrozen proposalMeta)

            -- Advance one voting period to a voting stage.
            advanceLevel period
            -- Then we send 50 upvotes for the proposal (as min quorum is 1% of total frozen tokens)
            let proposalKey = makeProposalKey (ProposeParams wallet1 requiredFrozen proposalMeta)
            withSender voter1 $
              call baseDao (Call @"Vote") [PermitProtected (VoteParam proposalKey True 50 voter1) Nothing]

            -- Advance one voting period to a proposing stage.
            advanceLevel (period + 1)
            withSender admin $
              call baseDao (Call @"Flush") (1 :: Natural)

            consumer <- chAddress <$> originateSimple "consumer" [] (contractConsumer @(MText, (Maybe MText)))

            withSender voter1 $
              call baseDao (Call @"CallCustom") ([mt|lookup_registry|], lPackValueRaw ([mt|key|], consumer))

            --checkStorage (unTAddress consumer) (toVal [([mt|key|], Just [mt|testVal|])])

    , nettestScenarioOnEmulatorCaps "checks it can flush a transfer type proposal (#66)" $
        withOriginated 3
          (\(admin : _) ->
            setExtra @Natural [mt|max_proposal_size|] 200 $
            initialStorageWithExplictRegistryDAOConfig admin) $
          \[admin, wallet1, wallet2] (toPeriod -> period) baseDao dodTokenContract -> do

            let
              proposalMeta = lPackValueRaw @RegistryDaoProposalMetadata $
                Transfer_proposal $ TransferProposal
                  1
                  [ tokenTransferType (toAddress dodTokenContract) wallet2 wallet1]
                  []
              proposalSize = metadataSize proposalMeta
              proposeParams = ProposeParams wallet1 proposalSize proposalMeta

            withSender wallet1 $
              call baseDao (Call @"Freeze") (#amount .! proposalSize)
            withSender wallet2 $
              call baseDao (Call @"Freeze") (#amount .! 20)

            -- Advance one voting period to a proposing stage.
            advanceLevel period

            withSender wallet1 $
              call baseDao (Call @"Propose") proposeParams

            checkBalanceEmulator (unTAddress baseDao) wallet1 proposalSize

            let
              key1 = makeProposalKey proposeParams
              upvote = NoPermit VoteParam
                  { vFrom = wallet2
                  , vVoteType = True
                  , vVoteAmount = 20
                  , vProposalKey = key1
                  }

            -- Advance one voting period to a voting stage.
            advanceLevel period
            withSender wallet2 $ call baseDao (Call @"Vote") [upvote]
            -- Advance one voting period to a proposing stage.
            advanceLevel (period + 1)
            withSender admin $ call baseDao (Call @"Flush") (1 :: Natural)

            checkBalanceEmulator (unTAddress baseDao) wallet1 proposalSize
            checkBalanceEmulator (unTAddress baseDao) wallet2 20

            checkStorage (unTAddress dodTokenContract)
              ( [ [ FA2.TransferItem { tiFrom = wallet2, tiTxs = [FA2.TransferDestination { tdTo = wallet1 , tdTokenId = FA2.theTokenId, tdAmount = 10 }] } ] -- Actual transfer
                , [ FA2.TransferItem { tiFrom = wallet2, tiTxs = [FA2.TransferDestination { tdTo = unTAddress baseDao, tdTokenId = FA2.theTokenId, tdAmount = 20 }] } ] -- Wallet2 freezes 20 tokens
                , [ FA2.TransferItem { tiFrom = wallet1, tiTxs = [FA2.TransferDestination { tdTo = unTAddress baseDao, tdTokenId = FA2.theTokenId, tdAmount = proposalSize }] } ] -- governance token transfer for freeze
                ])

    , nettestScenarioOnEmulatorCaps "checks it can propose a valid xtz type proposal (#66)" $
        withOriginated 2
          (\(admin : _) ->
            setExtra @Natural [mt|max_proposal_size|] 200 $
            initialStorageWithExplictRegistryDAOConfig admin) $
          \[_, wallet] (toPeriod -> period) baseDao _ -> do
            let
              proposalMeta amt = lPackValueRaw @RegistryDaoProposalMetadata $
                Transfer_proposal $
                  TransferProposal 1 [ xtzTransferType amt wallet ] []
              proposeParams amt = ProposeParams wallet (metadataSize $ proposalMeta amt) $ proposalMeta amt
              proposalSize amt = metadataSize $ proposalMeta amt

            withSender wallet $
              call baseDao (Call @"Freeze") (#amount .! proposalSize 10)

            -- Advance one voting period to a proposing stage.
            advanceLevel period

            -- Fails because 10 >= max_xtz_amount
            withSender wallet $
              call baseDao (Call @"Propose") (proposeParams 10)
              & expectFailProposalCheck tooLargeXtzErrMsg

            -- Fails because 1 >= min_xtz_amount
            withSender wallet $
              call baseDao (Call @"Propose") (proposeParams 1)
              & expectFailProposalCheck tooSmallXtzErrMsg

            withSender wallet $
              call baseDao (Call @"Freeze") (#amount .! proposalSize 3)

            -- Advance two voting period to another proposing stage.
            advanceLevel (2*period)

            withSender wallet $
              call baseDao (Call @"Propose") (proposeParams 3)

            checkBalanceEmulator (unTAddress baseDao) wallet (proposalSize 3 + proposalSize 10)


    , nettestScenarioOnEmulatorCaps "can flush a transfer proposal with registry updates" $
        withOriginated 3
          (\(admin : _) ->
            setExtra @Natural [mt|max_proposal_size|] 200 $
            initialStorageWithExplictRegistryDAOConfig admin) $
          \[admin, wallet1, wallet2] (toPeriod -> period) baseDao dodTokenContract -> do

            let
              proposalMeta = lPackValueRaw @RegistryDaoProposalMetadata $
                Transfer_proposal $ TransferProposal
                  1
                  [ tokenTransferType (toAddress dodTokenContract) wallet2 wallet1 ]
                  [ ([mt|testKey|], Just [mt|testValue|]) ]
              proposalSize = metadataSize proposalMeta
              proposeParams = ProposeParams wallet1 proposalSize proposalMeta

            withSender wallet1 $
              call baseDao (Call @"Freeze") (#amount .! proposalSize)
            withSender wallet2 $
              call baseDao (Call @"Freeze") (#amount .! 50)

            -- Advance one voting period to a proposing stage.
            advanceLevel period

            -- We propose the addition of a new registry key *and* a token transfer
            withSender wallet1 $
              call baseDao (Call @"Propose") proposeParams

            checkBalanceEmulator (unTAddress baseDao) wallet1 proposalSize

            -- Advance one voting period to a voting stage.
            advanceLevel period
            -- Then we send 50 upvotes for the proposal (as min quorum is 1% of total frozen tokens)
            let proposalKey = makeProposalKey (ProposeParams wallet1 proposalSize proposalMeta)
            withSender wallet2 $
              call baseDao (Call @"Vote") [PermitProtected (VoteParam proposalKey True 50 wallet2) Nothing]

            advanceLevel (period+1)
            withSender admin $ call baseDao (Call @"Flush") (1 :: Natural)

            -- check the registry update
            consumer <- chAddress <$> originateSimple "consumer" [] (contractConsumer @(MText, (Maybe MText)))
            withSender wallet2 $ call baseDao (Call @"CallCustom")
              ([mt|lookup_registry|], lPackValueRaw ([mt|testKey|], consumer))
            checkStorage consumer ([([mt|testKey|], Just [mt|testValue|])])

            -- check the balance
            checkBalanceEmulator (unTAddress baseDao) wallet1 proposalSize
            checkBalanceEmulator (unTAddress baseDao) wallet2 50

            checkStorage (unTAddress dodTokenContract)
              [ [ FA2.TransferItem { tiFrom = wallet2, tiTxs = [FA2.TransferDestination { tdTo = wallet1 , tdTokenId = FA2.theTokenId, tdAmount = 10 }] } ] -- Actual transfer
                , [ FA2.TransferItem { tiFrom = wallet2, tiTxs = [FA2.TransferDestination { tdTo = unTAddress baseDao, tdTokenId = FA2.theTokenId, tdAmount = 50 }] } ] -- Wallet2 freezes 50 tokens
                , [ FA2.TransferItem { tiFrom = wallet1, tiTxs = [FA2.TransferDestination { tdTo = unTAddress baseDao, tdTokenId = FA2.theTokenId, tdAmount = proposalSize }] } ] -- governance token transfer for freeze
              ]

    , nettestScenarioOnEmulatorCaps "checks it can transfer with receive_xtz_entrypoint (#66)" $
        withOriginated 3
          (\(admin : _) ->
            setExtra @Natural [mt|max_proposal_size|] 200 $
            initialStorageWithExplictRegistryDAOConfig admin) $
          \[admin, wallet1, wallet2] (toPeriod -> period) baseDao _ -> do
            sendXtz (toAddress baseDao) (unsafeBuildEpName "callCustom") ([mt|receive_xtz|], lPackValueRaw ())

            let
              proposalMeta = lPackValueRaw @RegistryDaoProposalMetadata $
                Transfer_proposal $
                  TransferProposal 1 [ xtzTransferType 3 wallet2 ] []
              proposalSize = metadataSize proposalMeta
              proposeParams = ProposeParams wallet1 proposalSize proposalMeta

            withSender wallet1 $
              call baseDao (Call @"Freeze") (#amount .! proposalSize)
            withSender wallet2 $
              call baseDao (Call @"Freeze") (#amount .! 10)

            -- Advance one voting period to a proposing stage.
            advanceLevel period

            withSender wallet1 $
              call baseDao (Call @"Propose") proposeParams
            let key1 = makeProposalKey proposeParams

            checkBalanceEmulator (unTAddress baseDao) wallet1 proposalSize


            let
              upvote = NoPermit VoteParam
                { vFrom = wallet2
                , vVoteType = True
                , vVoteAmount = 2
                , vProposalKey = key1
                }

            -- Advance one voting period to a voting stage.
            advanceLevel period
            withSender wallet2 $ call baseDao (Call @"Vote") [upvote]
            -- Advance one voting period to a proposing stage.
            advanceLevel (period+1)
            withSender admin $ call baseDao (Call @"Flush") (1 :: Natural)

    , nettestScenarioOnEmulatorCaps "checks it can flush a proposal that updates guardian address" $
        withOriginated 4
          (\(admin : _) ->
            setExtra @Natural [mt|max_proposal_size|] 200 $
            initialStorageWithExplictRegistryDAOConfig admin) $
          \[admin, wallet1, wallet2, newGuardian] (toPeriod -> period) baseDao _ -> do

            let
              proposalMeta = lPackValueRaw @RegistryDaoProposalMetadata $
                Update_guardian newGuardian

              proposalSize = metadataSize proposalMeta
              proposeParams = ProposeParams wallet1 proposalSize proposalMeta

            withSender wallet1 $
              call baseDao (Call @"Freeze") (#amount .! proposalSize)
            withSender wallet2 $
              call baseDao (Call @"Freeze") (#amount .! 20)

            -- Advance one voting period to a proposing stage.
            advanceLevel period

            withSender wallet1 $
              call baseDao (Call @"Propose") proposeParams

            checkBalanceEmulator (unTAddress baseDao) wallet1 proposalSize

            let
              key1 = makeProposalKey proposeParams
              upvote = NoPermit VoteParam
                  { vFrom = wallet2
                  , vVoteType = True
                  , vVoteAmount = 20
                  , vProposalKey = key1
                  }

            -- Advance one voting period to a voting stage.
            advanceLevel period
            withSender wallet2 $ call baseDao (Call @"Vote") [upvote]
            -- Advance one voting period to a proposing stage.
            advanceLevel (period + 1)
            withSender admin $ call baseDao (Call @"Flush") (1 :: Natural)

            checkGuardianEmulator (unTAddress baseDao) newGuardian
    ]
  ]
  where

    metadataSize :: ProposalMetadata -> Natural
    metadataSize = fromIntegral . BS.length

    -- Here we parse the storage value from compiled ligo storage, which
    -- contains the RegistryDAO lambdas implemented in LIGO, and we just use
    -- `fromVal` to convert it to a 'FullStorage'. Then we can set the
    -- RegistryDAO configuration values using the setExtra function below, and
    -- initialize the contract using it. This let us have the lambdas from LIGO
    -- in storage, and allows to tweak RegistryDAO configuration in tests.
    initialStorage :: Address -> FullStorage
    initialStorage admin = let
      fs = fromVal ($(fetchValue @FullStorage "haskell/test/registryDAO_storage.tz" "REGISTRY_STORAGE_PATH"))
      oldStorage = fsStorage fs
      oldConfig = fsConfig fs

      newStorage = oldStorage
        { sAdmin = admin
        }
      in fs { fsStorage = newStorage
            , fsConfig = oldConfig
                { cPeriod = 11
                , cProposalFlushTime = 22
                , cProposalExpiredTime = 33
                , cGovernanceTotalSupply = 100
                }
            }

    initialStorageWithExplictRegistryDAOConfig :: Address -> FullStorage
    initialStorageWithExplictRegistryDAOConfig admin =
      setExtra @Natural [mt|frozen_scale_value|] 1 $
      setExtra @Natural [mt|frozen_extra_value|] 0 $
      setExtra @Natural [mt|slash_scale_value|] 1 $
      setExtra @Natural [mt|slash_division_value|] 1 $
      setExtra @Natural [mt|min_xtz_amount|] 2 $
      setExtra @Natural [mt|max_xtz_amount|] 5 $
      setExtra @Natural [mt|max_proposal_size|] 100 (initialStorage admin)


expectFailProposalCheck
  :: (MonadNettest caps base m)
  => MText -> m a -> m ()
expectFailProposalCheck =
  expectCustomError #fAIL_PROPOSAL_CHECK

--------------------------------------------------------------------------
-- Helper
--------------------------------------------------------------------------

xtzTransferType :: Word32 -> Address -> TransferType
xtzTransferType amt toAddr = Xtz_transfer_type XtzTransfer
  { xtAmount = toMutez amt
  , xtRecipient = toAddr
  }

tokenTransferType :: Address -> Address -> Address -> TransferType
tokenTransferType contractAddr fromAddr toAddr = Token_transfer_type TokenTransfer
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
