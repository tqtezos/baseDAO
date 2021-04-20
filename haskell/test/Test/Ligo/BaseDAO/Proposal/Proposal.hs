-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Contains tests on @propose@ entrypoin logic for testing the Ligo contract.
module Test.Ligo.BaseDAO.Proposal.Proposal
  ( validProposal
  , rejectProposal
  , nonUniqueProposal
  , voteValidProposal
  ) where

import Universum

import Time (sec)

import Lorentz hiding ((>>))
import Morley.Nettest
import Util.Named

import Ligo.BaseDAO.Types
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Test.Ligo.BaseDAO.Common
import Test.Ligo.BaseDAO.Proposal.Config

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

validProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
validProposal originateFn = do
  ((owner1, _), _, dao, tokenContract, _) <- originateFn testConfig
  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = proposalMetadataFromNum 1
        }

  advanceTime (sec 10)
  withSender (AddressResolved owner1) $
    call dao (Call @"Freeze") (#amount .! 10, #keyhash .! (addressToKeyHash owner1))
  -- Check the token contract got a transfer call from
  -- baseDAO
  checkStorage (AddressResolved $ unTAddress tokenContract)
    (toVal [[FA2.TransferItem { tiFrom = owner1, tiTxs = [FA2.TransferDestination { tdTo = unTAddress dao, tdTokenId = FA2.theTokenId, tdAmount = 10 }] }]])
  advanceTime (sec 10)

  withSender (AddressResolved owner1) $ call dao (Call @"Propose") params
  checkTokenBalance frozenTokenId dao owner1 110

  -- Check total supply
  withSender (AddressResolved owner1) $
    call dao (Call @"Get_total_supply") (mkVoid frozenTokenId)
      & expectError dao (VoidResult (210 :: Natural)) -- initial = 0

rejectProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
rejectProposal originateFn = do
  ((owner1, _), _, dao, _, _) <- originateFn testConfig
  advanceTime (sec 10)
  let params = ProposeParams
        { ppFrozenToken = 9
        , ppProposalMetadata = proposalMetadataFromNum 1
        }

  withSender (AddressResolved owner1) $
    call dao (Call @"Freeze") (#amount .! 10, #keyhash .! (addressToKeyHash owner1))
  advanceTime (sec 10)

  withSender (AddressResolved owner1) $ call dao (Call @"Propose") params
    & expectCustomErrorNoArg #fAIL_PROPOSAL_CHECK dao

nonUniqueProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
nonUniqueProposal originateFn = do
  ((owner1, _), _, dao, _, _) <- originateFn testConfig
  advanceTime (sec 10)
  _ <- createSampleProposal 1 10 owner1 dao
  createSampleProposal 1 10 owner1 dao
    & expectCustomErrorNoArg #pROPOSAL_NOT_UNIQUE dao

voteValidProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
voteValidProposal originateFn = do
  ((owner1, _), (owner2, _), dao, _, _) <- originateFn voteConfig
  advanceTime (sec 120)

  withSender (AddressResolved owner2) $
    call dao (Call @"Freeze") (#amount .! 2, #keyhash .! (addressToKeyHash owner2))

  -- Create sample proposal (first proposal has id = 0)
  key1 <- createSampleProposal 1 120 owner1 dao
  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  advanceTime (sec 120)
  withSender (AddressResolved owner2) $ call dao (Call @"Vote") [params]
  checkTokenBalance frozenTokenId dao owner2 102
  -- TODO [#31]: check if the vote is updated properly
