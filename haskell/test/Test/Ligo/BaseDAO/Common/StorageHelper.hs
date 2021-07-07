-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
module Test.Ligo.BaseDAO.Common.StorageHelper
  ( checkBalance
  , checkGuardian
  , checkIfAProposalExist
  , getFreezeHistory
  , getFrozenTotalSupply
  , getFullStorage
  , getProposal
  , getQtAtCycle
  , getStorageRPC
  , getVotePermitsCounter
  ) where

import Lorentz hiding (assert, (>>))
import Universum

import qualified Data.Map as M
import Morley.Nettest
import Morley.Nettest.Pure (PureM)

import Ligo.BaseDAO.Types


getStorageRPC :: forall p base caps m. MonadNettest caps base m => TAddress p ->  m FullStorageRPC
getStorageRPC addr = getStorage @FullStorage (unTAddress addr)

------------------------------------------------------------------------
-- GetFrozenTotalSupplyFn
------------------------------------------------------------------------

getFrozenTotalSupply :: forall p base caps m. MonadNettest caps base m => TAddress p -> m Natural
getFrozenTotalSupply addr = (sFrozenTotalSupplyRPC . fsStorageRPC) <$> (getStorageRPC addr)

------------------------------------------------------------------------
-- GetFreezeHistoryFn
------------------------------------------------------------------------

getFreezeHistory :: forall p base caps m. MonadNettest caps base m => TAddress p -> Address -> m (Maybe AddressFreezeHistory)
getFreezeHistory addr owner = do
  freezeHistoryBmId <- (sFreezeHistoryRPC . fsStorageRPC) <$> (getStorageRPC addr)
  getBigMapValueMaybe freezeHistoryBmId owner

getQtAtCycle :: forall p base caps m. MonadNettest caps base m => TAddress p -> m QuorumThresholdAtCycle
getQtAtCycle addr = (sQuorumThresholdAtCycleRPC . fsStorageRPC) <$> getStorageRPC addr

getProposal
  :: forall p base caps m. MonadNettest caps base m
  => TAddress p
  -> ProposalKey
  -> m (Maybe Proposal)
getProposal addr pKey = do
  bId <- (sProposalsRPC . fsStorageRPC) <$> getStorageRPC addr
  getBigMapValueMaybe bId pKey

-- TODO: Implement this via [#31] instead
checkIfAProposalExist
  :: forall p base caps m. MonadNettest caps base m
  => ProposalKey -> TAddress p -> Bool -> m ()
checkIfAProposalExist proposalKey dodDao expected =
  getProposal dodDao proposalKey >>= (flip assert "Proposal not found" . (== expected) . isJust)

checkGuardian :: forall p base caps m. MonadNettest caps base m => TAddress p -> Address -> m ()
checkGuardian addr guardianToChk = do
  actual <- (sGuardianRPC . fsStorageRPC) <$> (getStorageRPC addr)
  actual @== guardianToChk

-- | Note: Not needed at the moment, due to all the tests that uses this run only in emulator
-- anyway. Commented due to weeder.

------------------------------------------------------------------------
-- CheckBalanceFn
------------------------------------------------------------------------

checkBalance
  :: forall p base caps m. MonadNettest caps base m
  => TAddress p
  -> Address
  -> Natural
  -> m ()
checkBalance addr owner bal = do
  fh <- getFreezeHistory addr owner
  (sumAddressFreezeHistory <$> fh) @== Just bal

sumAddressFreezeHistory :: AddressFreezeHistory -> Natural
sumAddressFreezeHistory AddressFreezeHistory{..} = fhCurrentUnstaked + fhPastUnstaked + fhStaked

------------------------------------------------------------------------
-- GetVotePermitsCounter
------------------------------------------------------------------------

getVotePermitsCounter :: forall p base caps m. MonadNettest caps base m => TAddress p ->  m Nonce
getVotePermitsCounter addr =
  (sPermitsCounterRPC . fsStorageRPC) <$> getStorageRPC addr

-- | Note: Not needed at the moment, due to all the tests that uses this run only in emulator
-- anyway. Commented due to weeder.

-- getVotePermitsCounterNetwork :: (Monad m) => Address -> NettestT m Nonce
-- getVotePermitsCounterNetwork addr = do
--   fs <- getFullStorageView addr
--   pure $ sPermitsCounter (fsStorage fs)
