-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
module Test.Ligo.BaseDAO.Common.StorageHelper
  ( checkBalance
  , checkGuardian
  , checkIfAProposalExist
  , checkIfDelegateExists
  , getFreezeHistory
  , getFrozenTotalSupply
  , getFullStorage
  , getProposal
  , getQtAtCycle
  , getStorageRPC
  , getVotePermitsCounter
  , getOriginationLevel
  ) where

import Lorentz hiding (assert, (>>))
import Universum

import qualified Data.Set as S
import Morley.Nettest

import Ligo.BaseDAO.Types


getStorageRPC :: forall p base caps m. MonadNettest caps base m => TAddress p ->  m FullStorageRPC
getStorageRPC addr = getStorage @FullStorage (unTAddress addr)

getFrozenTotalSupply :: forall p base caps m. MonadNettest caps base m => TAddress p -> m Natural
getFrozenTotalSupply addr = (sFrozenTotalSupplyRPC . fsStorageRPC) <$> (getStorageRPC addr)

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

checkIfDelegateExists
  :: forall p base caps m. MonadNettest caps base m
  => TAddress p
  -> Delegate
  -> m Bool
checkIfDelegateExists addr delegate = do
  bId <- (sDelegatesRPC . fsStorageRPC) <$> getStorageRPC addr
  isJust <$> getBigMapValueMaybe bId delegate

checkIfAProposalExist
  :: forall p base caps m. MonadNettest caps base m
  => ProposalKey -> TAddress p -> Bool -> m ()
checkIfAProposalExist proposalKey dodDao expected = do
  found <- getProposal dodDao proposalKey >>= \case
    Nothing -> pure False
    Just p -> do
      proposalSet <- (sProposalKeyListSortByDateRPC . fsStorageRPC) <$> getStorageRPC dodDao
      pure $ S.member (plStartLevel p, proposalKey) proposalSet
  assert (found == expected) $
    "Unexpected proposal status, expected:" <> (show expected) <> ", found: " <> (show found)

checkGuardian :: forall p base caps m. MonadNettest caps base m => TAddress p -> Address -> m ()
checkGuardian addr guardianToChk = do
  actual <- (sGuardianRPC . fsStorageRPC) <$> (getStorageRPC addr)
  actual @== guardianToChk

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

getVotePermitsCounter :: forall p base caps m. MonadNettest caps base m => TAddress p ->  m Nonce
getVotePermitsCounter addr =
  (sPermitsCounterRPC . fsStorageRPC) <$> getStorageRPC addr

getOriginationLevel :: forall p base caps m. MonadNettest caps base m => TAddress p ->  m Natural
getOriginationLevel dodDao = (sStartLevelRPC . fsStorageRPC) <$> (getStorageRPC dodDao)
