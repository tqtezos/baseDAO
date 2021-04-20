-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Contains test on @vote@ entrypoint for the LIGO contract.
module Test.Ligo.BaseDAO.Proposal.Vote
  ( voteNonExistingProposal
  , voteMultiProposals
  , voteOutdatedProposal
  ) where

import Universum

import Lorentz hiding ((>>))
import Lorentz.Test hiding (withSender)
import Morley.Nettest
import Util.Named

import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.BaseDAO.Proposal.Config

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

voteNonExistingProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
voteNonExistingProposal originateFn = do
  ((owner1, _), (owner2, _), dao, _, _) <- originateFn testConfig
  advanceTime (sec 10)

  withSender (AddressResolved owner2) $
    call dao (Call @"Freeze") (#amount .! 2, #keyhash .! (addressToKeyHash owner2))

  -- Create sample proposal
  _ <- createSampleProposal 1 15 owner1 dao
  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = HashUnsafe "\11\12\13"
        }

  withSender (AddressResolved owner2) $ call dao (Call @"Vote") [params]
    & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST dao

voteMultiProposals
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
voteMultiProposals originateFn = do
  ((owner1, _), (owner2, _), dao, _, _) <- originateFn voteConfig

  advanceTime (sec 120)
  withSender (AddressResolved owner1) $
    call dao (Call @"Freeze") (#amount .! 20, #keyhash .! (addressToKeyHash owner1))

  withSender (AddressResolved owner2) $
    call dao (Call @"Freeze") (#amount .! 5, #keyhash .! (addressToKeyHash owner2))
  advanceTime (sec 120)

  -- Create sample proposal
  key1 <- createSampleProposal 1 0 owner1 dao
  key2 <- createSampleProposal 2 0 owner1 dao
  let params = fmap NoPermit
        [ VoteParam
            { vVoteType = True
            , vVoteAmount = 2
            , vProposalKey = key1
            }
        , VoteParam
            { vVoteType = False
            , vVoteAmount = 3
            , vProposalKey = key2
            }
        ]

  advanceTime (sec 120)
  withSender (AddressResolved owner2) $ call dao (Call @"Vote") params
  checkTokenBalance (frozenTokenId) dao owner2 105
  -- TODO [#31]: check storage if the vote update the proposal properly

voteOutdatedProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
voteOutdatedProposal originateFn = do
  ((owner1, _), (owner2, _), dao, _, _) <- originateFn testConfig
  advanceTime (sec 10)

  withSender (AddressResolved owner2) $
    call dao (Call @"Freeze") (#amount .! 2, #keyhash .! (addressToKeyHash owner2))
  -- Create sample proposal
  key1 <- createSampleProposal 1 10 owner1 dao

  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        }

  advanceTime (sec 10)
  withSender (AddressResolved owner2) $ do
    call dao (Call @"Vote") [params]
    advanceTime (sec 25)
    call dao (Call @"Vote") [params]
      & expectCustomErrorNoArg #vOTING_PERIOD_OVER dao
