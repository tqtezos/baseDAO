-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.TreasuryDAO
  ( test_TreasuryDAO
  ) where

import Universum

import qualified Data.ByteString as BS
import Lorentz
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Michelson.Untyped.Entrypoints
import Morley.Nettest
import Morley.Nettest.Tasty
import Test.Tasty (TestTree, testGroup)
import Util.Named

import Ligo.BaseDAO.Common.Types
import Ligo.BaseDAO.Types
import Ligo.Util
import Test.Ligo.BaseDAO.Common

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

data TransferProposal = TransferProposal
  { tpAgoraPostId :: Natural
  , tpTransfers :: [TransferType]
  }

instance HasAnnotation TransferProposal where
  annOptions = baseDaoAnnOptions

customGeneric "TransferProposal" ligoLayout

deriving anyclass instance IsoValue TransferProposal

-- | Helper type for unpack/pack
data TreasuryDaoProposalMetadata
  = Transfer_proposal TransferProposal
  | Update_guardian Address

instance HasAnnotation TreasuryDaoProposalMetadata where
  annOptions = baseDaoAnnOptions

customGeneric "TreasuryDaoProposalMetadata" ligoLayout
deriving anyclass instance IsoValue TreasuryDaoProposalMetadata

-- | Testing a Treasury-like DAO. Ex. DNS Treasury
test_TreasuryDAO :: TestTree
test_TreasuryDAO = testGroup "TreasuryDAO Tests"
  [ testGroup "Proposal creator:"
      [ nettestScenarioOnEmulatorCaps "can propose a valid proposal" $
          validProposal checkBalanceEmulator
      , nettestScenarioOnEmulatorCaps "can flush a Token transfer proposal" $
          flushTokenTransfer checkBalanceEmulator
      , nettestScenarioOnEmulatorCaps "can flush a Xtz transfer proposal" $
          flushXtzTransfer checkBalanceEmulator
      , nettestScenarioOnEmulatorCaps "can flush a Update_guardian proposal" $
          flushUpdateGuardian checkGuardianEmulator
      ]

  , testGroup "proposal_check:"
      [ nettestScenarioOnEmulatorCaps "fail when xtz transfer contains 0 mutez" $
          proposalCheckFailZeroMutez
      , nettestScenarioOnEmulatorCaps "fail when proposal size is bigger than max" $
          proposalCheckBiggerThanMaxProposalSize
      ]
  ]


metadataSize :: ByteString -> Natural
metadataSize md = fromIntegral $ BS.length md

validProposal
  :: forall caps base m. (MonadNettest caps base m, HasCallStack)
  => CheckBalanceFn m -> m ()
validProposal checkBalanceFn = withFrozenCallStack $ do
  DaoOriginateData{..} <- originateTreasuryDao id defaultQuorumThreshold
  let
    proposalMeta = lPackValueRaw @TreasuryDaoProposalMetadata $
      Transfer_proposal $ TransferProposal
        { tpAgoraPostId = 1
        , tpTransfers = [ tokenTransferType (toAddress dodDao) dodOwner1 dodOwner2 ]
        }
    proposalSize = metadataSize proposalMeta -- 115

  -- Freeze in voting stage.
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! proposalSize)

  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod

  withSender dodOwner1 $
    call dodDao (Call @"Propose") (ProposeParams dodOwner1 (proposalSize + 1) proposalMeta)
    & expectCustomError #fAIL_PROPOSAL_CHECK incorrectTokenAmountErrMsg

  withSender dodOwner1 $
    call dodDao (Call @"Propose") (ProposeParams dodOwner1 proposalSize proposalMeta)

  checkBalanceFn (unTAddress dodDao) dodOwner1 (proposalSize)

flushTokenTransfer
  :: forall caps base m. (MonadNettest caps base m, HasCallStack)
  => CheckBalanceFn m -> m ()
flushTokenTransfer checkBalanceFn = withFrozenCallStack $ do
  DaoOriginateData{..} <- originateTreasuryDao id defaultQuorumThreshold

  let
    proposalMeta = lPackValueRaw @TreasuryDaoProposalMetadata $
      Transfer_proposal $ TransferProposal
        { tpAgoraPostId = 1
        , tpTransfers = [ tokenTransferType (toAddress dodTokenContract) dodOwner2 dodOwner1 ]
        }
    proposalSize = metadataSize proposalMeta
    proposeParams = ProposeParams dodOwner1 proposalSize proposalMeta

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! proposalSize)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 20)

  -- Advance one voting periods to a proposing stage.
  advanceLevel dodPeriod

  withSender dodOwner1 $ call dodDao (Call @"Propose") proposeParams
  let key1 = makeProposalKey proposeParams

  checkBalanceFn (unTAddress dodDao) dodOwner1 proposalSize

  let
    upvote = NoPermit VoteParam
        { vFrom = dodOwner2
        , vVoteType = True
        , vVoteAmount = 20
        , vProposalKey = key1
        }

  -- Advance one voting period to a voting stage.
  advanceLevel dodPeriod
  withSender dodOwner2 $ call dodDao (Call @"Vote") [upvote]
  -- Advance one voting period to a proposing stage.
  advanceLevel $ dodPeriod + 1 -- meet `proposal_flush_time`
  withSender dodAdmin $ call dodDao (Call @"Flush") 100

  checkBalanceFn (unTAddress dodDao) dodOwner1 proposalSize
  checkBalanceFn (unTAddress dodDao) dodOwner2 20

flushXtzTransfer
  :: forall caps base m. (MonadNettest caps base m, HasCallStack)
  => CheckBalanceFn m -> m ()
flushXtzTransfer checkBalanceFn = withFrozenCallStack $ do
  DaoOriginateData{..} <- originateTreasuryDao id defaultQuorumThreshold

  sendXtz (toAddress dodDao) (unsafeBuildEpName "callCustom") ([mt|receive_xtz|], lPackValueRaw ())

  let
    proposalMeta amt = lPackValueRaw @TreasuryDaoProposalMetadata $
      Transfer_proposal $ TransferProposal
        { tpAgoraPostId = 1
        , tpTransfers = [ xtzTransferType amt dodOwner2 ]
        }
    proposeParams amt = ProposeParams dodOwner1 (metadataSize $ proposalMeta amt) $ proposalMeta amt

  -- Freeze in initial voting stage.
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! (metadataSize $ proposalMeta 3))

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 10)
  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod

  withSender dodOwner1 $ do
  -- due to smaller than min_xtz_amount
    call dodDao (Call @"Propose") (proposeParams 1)
      & expectCustomError #fAIL_PROPOSAL_CHECK tooSmallXtzErrMsg

  -- due to bigger than max_xtz_amount
    call dodDao (Call @"Propose") (proposeParams 6)
      & expectCustomError #fAIL_PROPOSAL_CHECK tooLargeXtzErrMsg

    call dodDao (Call @"Propose") (proposeParams 3)
  let key1 = makeProposalKey (proposeParams 3)

  checkBalanceFn (unTAddress dodDao) dodOwner1 45

  let
    upvote = NoPermit VoteParam
        { vFrom = dodOwner2
        , vVoteType = True
        , vVoteAmount = 1
        , vProposalKey = key1
        }

  -- Advance one voting period to a voting stage.
  advanceLevel dodPeriod
  withSender dodOwner2 $ call dodDao (Call @"Vote") [upvote]
  -- Advance one voting period to a proposing stage.
  advanceLevel $ dodPeriod + 1 -- meet `proposal_flush_time`
  withSender dodAdmin $ call dodDao (Call @"Flush") 100

  -- TODO: check xtz balance

flushUpdateGuardian
  :: forall caps base m. (MonadNettest caps base m, HasCallStack)
  => CheckGuardianFn m -> m ()
flushUpdateGuardian checkGuardian = withFrozenCallStack $ do
  DaoOriginateData{..} <- originateTreasuryDao id defaultQuorumThreshold

  sendXtz (toAddress dodDao) (unsafeBuildEpName "callCustom") ([mt|receive_xtz|], lPackValueRaw ())

  let
    proposalMeta = lPackValueRaw @TreasuryDaoProposalMetadata $
      Update_guardian dodOwner2
    proposeParams = ProposeParams dodOwner1 (metadataSize $ proposalMeta) $ proposalMeta

  -- Freeze in initial voting stage.
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! (metadataSize $ proposalMeta))

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 10)
  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod

  withSender dodOwner1 $
    call dodDao (Call @"Propose") proposeParams
  let key1 = makeProposalKey proposeParams

  let
    upvote = NoPermit VoteParam
        { vFrom = dodOwner2
        , vVoteType = True
        , vVoteAmount = 1
        , vProposalKey = key1
        }

  -- Advance one voting period to a voting stage.
  advanceLevel dodPeriod
  withSender dodOwner2 $ call dodDao (Call @"Vote") [upvote]
  -- Advance one voting period to a proposing stage.
  advanceLevel $ (dodPeriod + 1) -- meet `proposal_flush_level`
  withSender dodAdmin $ call dodDao (Call @"Flush") 100
  checkGuardian (unTAddress dodDao) dodOwner2

proposalCheckFailZeroMutez
  :: forall caps base m. (MonadNettest caps base m, HasCallStack)
  => m ()
proposalCheckFailZeroMutez = withFrozenCallStack do
  DaoOriginateData{..} <-
    originateTreasuryDao
      (\store -> setExtra @Natural [mt|min_xtz_amount|] 0 store)
      defaultQuorumThreshold

  let
    proposalMeta = lPackValueRaw @TreasuryDaoProposalMetadata $
      Transfer_proposal $ TransferProposal
        { tpAgoraPostId = 1
        , tpTransfers = [ xtzTransferType 0 dodOwner2 ]
        }
    proposalSize = metadataSize proposalMeta

  -- Freeze in voting stage.
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! proposalSize)

  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod

  withSender dodOwner1 $
    call dodDao (Call @"Propose") (ProposeParams dodOwner1 proposalSize proposalMeta)
      & expectCustomError #fAIL_PROPOSAL_CHECK zeroMutezErrMsg

proposalCheckBiggerThanMaxProposalSize
  :: forall caps base m. (MonadNettest caps base m, HasCallStack)
  => m ()
proposalCheckBiggerThanMaxProposalSize = withFrozenCallStack do
  DaoOriginateData{..} <-
    originateTreasuryDao id defaultQuorumThreshold
  let
    largeProposalMeta = lPackValueRaw @TreasuryDaoProposalMetadata $
      Transfer_proposal $ TransferProposal 1 $
        [tokenTransferType (toAddress dodDao) dodOwner1 dodOwner2 | (_ :: Integer) <- [1..10]]
    largeProposalSize = metadataSize largeProposalMeta

  -- Freeze in voting stage.
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! largeProposalSize)

  -- Advance one voting period to a proposing stage.
  advanceLevel 10

  withSender dodOwner1 $
    call dodDao (Call @"Propose") (ProposeParams dodOwner1 largeProposalSize largeProposalMeta)
      & expectCustomError #fAIL_PROPOSAL_CHECK tooLargeProposalErrMsg


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

originateTreasuryDao
 :: forall caps base m. (MonadNettest caps base m)
 => (FullStorage -> FullStorage)
 -> OriginateFn m
originateTreasuryDao modifyStorageFn =
  let fs = fromVal ($(fetchValue @FullStorage "haskell/test/treasuryDAO_storage.tz" "TREASURY_STORAGE_PATH"))
      FullStorage{..} = fs
        & setExtra @Natural [mt|frozen_scale_value|] 1
        & setExtra @Natural [mt|frozen_extra_value|] 0
        & setExtra @Natural [mt|slash_scale_value|] 1
        & setExtra @Natural [mt|slash_division_value|] 1
        & setExtra @Natural [mt|max_proposal_size|] 1000
        & setExtra @Natural [mt|min_xtz_amount|] 2
        & setExtra @Natural [mt|max_xtz_amount|] 5
        & modifyStorageFn

  in originateLigoDaoWithConfig (sExtra fsStorage)
      (fsConfig
        { cMinQuorumThreshold = fromIntegral $ mkQuorumThreshold 1 100
        , cPeriod = 10
        , cProposalFlushTime = 20
        , cProposalExpiredTime = 30
        }
      )
