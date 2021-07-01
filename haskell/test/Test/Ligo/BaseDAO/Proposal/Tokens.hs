-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Contains tests on @propose@ entrypoint logic for behavior around
-- tokens, freezing/unfreezing etc.
module Test.Ligo.BaseDAO.Proposal.Tokens
  ( canUnfreezeFromPreviousPeriod
  , cannotUnfreezeFromSamePeriod
  , cannotUnfreezeStakedTokens
  , checkFreezeHistoryTracking
  , freezeTokens
  ) where

import Universum

import qualified Data.ByteString as BS
import Lorentz hiding (assert, (>>))
import Morley.Nettest
import Util.Named

import Ligo.BaseDAO.Types
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Test.Ligo.BaseDAO.Common
import Test.Ligo.BaseDAO.Proposal.Config

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

freezeTokens
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> CheckBalanceFn m -> m ()
freezeTokens originateFn checkBalanceFn = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold

  withSender dodOwner1 $ call dodDao (Call @"Freeze") (#amount .! 10)
  checkBalanceFn (unTAddress dodDao) dodOwner1 10
  -- Check that the FA2 token got a transfer call as expected.

  tcStorage <- getStorage @[[FA2.TransferItem]] (unTAddress dodTokenContract)
  assert (tcStorage ==
    ([[FA2.TransferItem
      { tiFrom = dodOwner1
      , tiTxs = [FA2.TransferDestination { tdTo = unTAddress dodDao, tdTokenId = FA2.theTokenId, tdAmount = 10 }]
      }]])) "Unexpected FA2 transfers"

checkFreezeHistoryTracking
  :: (MonadEmulated caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m)
  -> GetFreezeHistoryFn m
  -> m ()
checkFreezeHistoryTracking originateFn getFreezeHistory = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold
  let frozen_scale_value = 2
  let frozen_extra_value = 10
  let proposalMeta1 = ""
  let proposalSize1 = fromIntegral . BS.length $ proposalMeta1
  let requiredFrozen = proposalSize1 * frozen_scale_value + frozen_extra_value

  withSender dodOwner1 $ call dodDao (Call @"Freeze") (#amount .! requiredFrozen)
  advanceLevel dodPeriod
  withSender dodOwner1 $ call dodDao (Call @"Propose") (ProposeParams dodOwner1 requiredFrozen proposalMeta1)
  advanceLevel dodPeriod

  fh <- getFreezeHistory (unTAddress dodDao) dodOwner1 -- TODO [#31]
  let expected = AddressFreezeHistory
        { fhCurrentStageNum = 1
        , fhCurrentUnstaked = 0
        , fhStaked = requiredFrozen
        , fhPastUnstaked = 0
        }

  assert (fh == (Just expected)) "Unexpected freeze history"

canUnfreezeFromPreviousPeriod
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> CheckBalanceFn m -> m ()
canUnfreezeFromPreviousPeriod originateFn checkBalanceFn = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold

  withSender dodOwner1 $ call dodDao (Call @"Freeze") (#amount .! 10)
  checkBalanceFn (unTAddress dodDao) dodOwner1 10

  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod

  withSender dodOwner1 $ call dodDao (Call @"Unfreeze") (#amount .! 10)
  checkBalanceFn (unTAddress dodDao) dodOwner1 00
  -- Check that the FA2 token got a transfer call as expected.
  tcStorage <- getStorage @[[FA2.TransferItem]] (unTAddress dodTokenContract)
  assert (tcStorage ==
    ([ [ FA2.TransferItem
        { tiFrom = unTAddress dodDao
        , tiTxs = [FA2.TransferDestination { tdTo = dodOwner1, tdTokenId = FA2.theTokenId, tdAmount = 10 }]
        }]
      , [FA2.TransferItem
        { tiFrom = dodOwner1
        , tiTxs = [FA2.TransferDestination { tdTo = unTAddress dodDao, tdTokenId = FA2.theTokenId, tdAmount = 10 }]
      }]])) "Unexpected FA2 transfers"

cannotUnfreezeStakedTokens
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> CheckBalanceFn m -> m ()
cannotUnfreezeStakedTokens originateFn checkBalanceFn = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold

  withSender dodOwner1 $ call dodDao (Call @"Freeze") (#amount .! 50)
  checkBalanceFn (unTAddress dodDao) dodOwner1 50

  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod
  void $ createSampleProposal 1 dodOwner1 dodDao

  -- the frozen tokens are still the same
  checkBalanceFn (unTAddress dodDao) dodOwner1 50
  -- but unfreeze won't let all of them be unfrozen because of the staked tokens
  -- note: 110 tokens are staked here
  withSender dodOwner1 $ call dodDao (Call @"Unfreeze") (#amount .! 41)
    & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS
  -- it will allow the un-staked ones to be unfrozen
  withSender dodOwner1 $ call dodDao (Call @"Unfreeze") (#amount .! 40)

cannotUnfreezeFromSamePeriod
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> CheckBalanceFn m -> m ()
cannotUnfreezeFromSamePeriod originateFn checkBalanceFn = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold

  withSender dodOwner1 $ call dodDao (Call @"Freeze") (#amount .! 10)
  checkBalanceFn (unTAddress dodDao) dodOwner1 10

  -- Cannot unfreeze in the same period
  withSender dodOwner1 $ call dodDao (Call @"Unfreeze") (#amount .! 10)
    & expectCustomError_ #nOT_ENOUGH_FROZEN_TOKENS

