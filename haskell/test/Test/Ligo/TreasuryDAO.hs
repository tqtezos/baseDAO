-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.TreasuryDAO
  ( test_TreasuryDAO
  ) where

import Universum

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Lorentz
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Michelson.Untyped.Entrypoints
import Morley.Nettest
import Morley.Nettest.Tasty
import Test.Tasty (TestTree, testGroup)
import Time (sec)
import Util.Named

import Ligo.BaseDAO.Common.Types
import Ligo.BaseDAO.Types
import Ligo.Util
import Test.Ligo.BaseDAO.Common
  (OriginateFn, addressToKeyHash, checkTokenBalance, makeProposalKey, originateLigoDaoWithBalance, sendXtz)

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- | Testing a Treasury-like DAO. Ex. DNS Treasury
test_TreasuryDAO :: TestTree
test_TreasuryDAO = testGroup "TreasuryDAO Tests"
  [ testGroup "Proposal creator:"
      [ nettestScenarioOnEmulator "can propose a valid proposal" $
          \_emulated -> validProposal
      , nettestScenarioOnEmulator "can flush a Token transfer proposal" $
          \_emulated -> flushTokenTransfer
      , nettestScenarioOnEmulator "can flush a Xtz transfer proposal" $
          \_emulated -> flushXtzTransfer

      -- -- TODO: flush a config proposal
      -- -- TODO: check all fail scenario of proposal_check
      ]
  ]


metadataSize :: DynamicRec "pm" -> Natural
metadataSize md = fromIntegral $ BS.length $ lPackValueRaw md

validProposal :: (Monad m, HasCallStack) => NettestImpl m -> m ()
validProposal = uncapsNettest $ withFrozenCallStack do
  ((owner1, _), (owner2, _), dao, _, _) <-
    originateTreasuryDaoWithBalance $ \owner1_ owner2_ ->
      [ ((owner1_, frozenTokenId), 200)
      , ((owner2_, frozenTokenId), 200)
      ]
  let
    proposalMeta = DynamicRec $ Map.fromList $
      [ ([mt|agora_post_id|], lPackValueRaw @Natural 1)
      , ([mt|transfers|], lPackValueRaw @([TransferType])
          [ tokenTransferType (toAddress dao) owner1 owner2
          ]
        )
      ]
    proposalSize = metadataSize proposalMeta

  withSender (AddressResolved owner1) $
    call dao (Call @"Freeze") (#amount .! proposalSize, #keyhash .! (addressToKeyHash owner1))

  -- Advance one voting period.
  advanceTime (sec 1.5)

  withSender (AddressResolved owner1) $
    call dao (Call @"Propose") (ProposeParams (proposalSize + 1) proposalMeta)
    & expectCustomErrorNoArg #fAIL_PROPOSAL_CHECK dao

  withSender (AddressResolved owner1) $
    call dao (Call @"Propose") (ProposeParams proposalSize proposalMeta)

  checkTokenBalance frozenTokenId dao owner1 366

flushTokenTransfer :: (Monad m, HasCallStack) => NettestImpl m -> m ()
flushTokenTransfer = uncapsNettest $ withFrozenCallStack $ do
  ((owner1, _), (owner2, _), dao, fa2Contract, admin) <-
    originateTreasuryDaoWithBalance $ \_ owner2_ ->
      [ ((owner2_, frozenTokenId), 100)
      ]

  let
    proposalMeta = DynamicRec $ Map.fromList $
      [ ([mt|agora_post_id|], lPackValueRaw @Natural 1)
      , ([mt|transfers|], lPackValueRaw @([TransferType])
          [ tokenTransferType (toAddress fa2Contract) owner2 owner1
          ]
        )
      ]
    proposalSize = metadataSize proposalMeta
    proposeParams = ProposeParams proposalSize proposalMeta

  withSender (AddressResolved owner1) $
    call dao (Call @"Freeze") (#amount .! proposalSize, #keyhash .! (addressToKeyHash owner1))

  withSender (AddressResolved owner2) $
    call dao (Call @"Freeze") (#amount .! 20, #keyhash .! (addressToKeyHash owner2))

  -- Advance one voting period (which is 1).
  advanceTime (sec 1.5)

  withSender (AddressResolved owner1) $
    call dao (Call @"Propose") proposeParams
  let key1 = makeProposalKey proposeParams owner1

  checkTokenBalance frozenTokenId dao owner1 166

  let
    upvote = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 20
        , vProposalKey = key1
        }

  advanceTime (sec 1)
  withSender (AddressResolved owner2) $ call dao (Call @"Vote") [upvote]
  advanceTime (sec 1)
  withSender (AddressResolved admin) $ call dao (Call @"Flush") 100

  checkTokenBalance frozenTokenId dao owner1 proposalSize
  checkTokenBalance frozenTokenId dao owner2 120

flushXtzTransfer :: (Monad m, HasCallStack) => NettestImpl m -> m ()
flushXtzTransfer = uncapsNettest $ withFrozenCallStack $ do
  ((owner1, _), (owner2, _), dao, _, admin) <-
    originateTreasuryDaoWithBalance $ \_ _ ->
      []

  sendXtz (toAddress dao) (unsafeBuildEpName "callCustom") ([mt|receive_xtz|], lPackValueRaw ())

  let
    proposalMeta amt = DynamicRec $ Map.fromList $
      [ ([mt|agora_post_id|], lPackValueRaw @Natural 1)
      , ([mt|transfers|], lPackValueRaw @([TransferType])
          [ xtzTransferType amt owner2 -- transfer from dao to owner2
          ]
        )
      ]
    proposeParams amt = ProposeParams (metadataSize $ proposalMeta amt) $ proposalMeta amt

  withSender (AddressResolved owner1) $
    call dao (Call @"Freeze") (#amount .! (metadataSize $ proposalMeta 3), #keyhash .! (addressToKeyHash owner1))

  withSender (AddressResolved owner2) $
    call dao (Call @"Freeze") (#amount .! 10, #keyhash .! (addressToKeyHash owner2))
  -- Advance one voting period.
  advanceTime (sec 1.5)

  withSender (AddressResolved owner1) $ do
  -- due to smaller than min_xtz_amount
    call dao (Call @"Propose") (proposeParams 1)
      & expectCustomErrorNoArg #fAIL_PROPOSAL_CHECK dao

  -- due to bigger than max_xtz_amount
    call dao (Call @"Propose") (proposeParams 6)
      & expectCustomErrorNoArg #fAIL_PROPOSAL_CHECK dao

    call dao (Call @"Propose") (proposeParams 3)
  let key1 = makeProposalKey (proposeParams 3) owner1

  checkTokenBalance (frozenTokenId) dao owner1 94

  let
    upvote = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 1
        , vProposalKey = key1
        }

  advanceTime (sec 1)
  withSender (AddressResolved owner2) $ call dao (Call @"Vote") [upvote]
  advanceTime (sec 1)
  withSender (AddressResolved admin) $ call dao (Call @"Flush") 100

--   -- TODO: check xtz balance

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
      , tiTxs = [ FA2.TransferDestination
          { tdTo = toAddr
          , tdTokenId = FA2.theTokenId
          , tdAmount = 10
          } ]
      } ]
  }

originateTreasuryDaoWithBalance
 :: forall caps base m. (MonadNettest caps base m)
 => (Address -> Address -> [(LedgerKey, LedgerValue)]) -> OriginateFn m
originateTreasuryDaoWithBalance bal =
  let fs = fromVal ($(fetchValue @FullStorage "haskell/test/treasuryDAO_storage.tz" "TREASURY_STORAGE_PATH"))
      testExtra = (sExtra $ fsStorage fs)
        & setExtra @Natural [mt|frozen_scale_value|] 1
        & setExtra @Natural [mt|frozen_extra_value|] 0
        & setExtra @Natural [mt|slash_scale_value|] 1
        & setExtra @Natural [mt|slash_division_value|] 1
        & setExtra @Natural [mt|max_proposal_size|] 1000
        & setExtra @Natural [mt|min_xtz_amount|] 2
        & setExtra @Natural [mt|max_xtz_amount|] 5

  in originateLigoDaoWithBalance testExtra (fsConfig fs) bal
  where
    setExtra :: forall a n. NicePackedValue a => MText -> a -> DynamicRec n -> DynamicRec n
    setExtra key v extra =
      DynamicRec $ Map.insert key (lPackValueRaw v) (unDynamic extra)
