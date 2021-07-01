-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# LANGUAGE ApplicativeDo #-}
-- | Contains test on @vote@ entrypoint for the LIGO contract.
module Test.Ligo.BaseDAO.Proposal.Vote
  ( proposalCorrectlyTrackVotes
  , voteNonExistingProposal
  , voteDeletedProposal
  , voteMultiProposals
  , voteOutdatedProposal
  , voteValidProposal
  , voteWithPermit
  , voteWithPermitNonce
  , votesBoundedValue
  ) where

import Universum

import qualified Data.Map as Map
import Lorentz hiding (assert, (>>))
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
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 2)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod
  -- Create sample proposal
  _ <- createSampleProposal 1 dodOwner1 dodDao
  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = UnsafeHash "\11\12\13"
        , vFrom = dodOwner2
        }
  -- Advance one voting period to a voting stage.
  advanceLevel dodPeriod

  withSender dodOwner2 $ call dodDao (Call @"Vote") [params]
    & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST

voteMultiProposals
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> CheckBalanceFn m -> m ()
voteMultiProposals originateFn checkBalanceFn = do
  DaoOriginateData{..} <- originateFn voteConfig defaultQuorumThreshold

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 20)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 5)

  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod

  -- Create sample proposal
  (key1, key2) <- createSampleProposals (1, 2) dodOwner1 dodDao
  let params = fmap NoPermit
        [ VoteParam
            { vVoteType = True
            , vVoteAmount = 2
            , vProposalKey = key1
            , vFrom = dodOwner2
            }
        , VoteParam
            { vVoteType = False
            , vVoteAmount = 3
            , vProposalKey = key2
            , vFrom = dodOwner2
            }
        ]

  -- Advance one voting period to a voting stage.
  advanceLevel dodPeriod
  withSender dodOwner2 $ call dodDao (Call @"Vote") params
  checkBalanceFn (unTAddress dodDao) dodOwner2 5
  -- TODO [#31]: check storage if the vote update the proposal properly

proposalCorrectlyTrackVotes
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m)
  -> GetProposalFn m
  -> m ()
proposalCorrectlyTrackVotes originateFn getProposalFn = do
  DaoOriginateData{..} <- originateFn voteConfig defaultQuorumThreshold

  let proposer = dodOwner1
  let voter1 = dodOwner2
  let voter2 = dodOperator1

  withSender proposer $
    call dodDao (Call @"Freeze") (#amount .! 20)

  withSender voter1 $
    call dodDao (Call @"Freeze") (#amount .! 40)

  withSender voter2 $
    call dodDao (Call @"Freeze") (#amount .! 40)

  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod

  -- Create sample proposal
  (key1, key2) <- createSampleProposals (1, 2) dodOwner1 dodDao
  let params1 = fmap NoPermit
        [ VoteParam
            { vVoteType = True
            , vVoteAmount = 5
            , vProposalKey = key1
            , vFrom = voter1
            }
        , VoteParam
            { vVoteType = False
            , vVoteAmount = 3
            , vProposalKey = key2
            , vFrom = voter1
            }
        ]

  let params2 = fmap NoPermit
        [ VoteParam
            { vVoteType = False
            , vVoteAmount = 2
            , vProposalKey = key1
            , vFrom = voter2
            }
        , VoteParam
            { vVoteType = True
            , vVoteAmount = 4
            , vProposalKey = key2
            , vFrom = voter2
            }
        ]

  let params3 = fmap NoPermit
        [ VoteParam
            { vVoteType = True
            , vVoteAmount = 3
            , vProposalKey = key1
            , vFrom = voter1
            }
        , VoteParam
            { vVoteType = True
            , vVoteAmount = 3
            , vProposalKey = key2
            , vFrom = voter1
            }
        ]

  -- Advance one voting period to a voting stage.
  advanceLevel dodPeriod
  withSender voter1 . inBatch $ do
    call dodDao (Call @"Vote") params1
    call dodDao (Call @"Vote") params3
    pure ()

  withSender voter2  do
    call dodDao (Call @"Vote") params2

  proposal1 <- fromMaybe (error "proposal not found") <$> getProposalFn (unTAddress dodDao) key1
  proposal2 <- fromMaybe (error "proposal not found") <$> getProposalFn (unTAddress dodDao) key2

  assert (plUpvotes proposal1 == 8) "proposal did not track upvotes correctly"
  assert (plDownvotes proposal1 == 2) "proposal did not track downvotes correctly"

  assert (plUpvotes proposal2 == 7) "proposal did not track upvotes correctly"
  assert (plDownvotes proposal2 == 3) "proposal did not track downvotes correctly"

  let proposal1Voters = plVoters proposal1
  let proposal2Voters = plVoters proposal2

  assert ((Map.lookup (voter1, True) proposal1Voters) == Just 8) $ "proposal did not track upvote count for voter correctly"
  assert (isNothing $ (Map.lookup (voter1, False) proposal1Voters)) $ "proposal did not track upvote count for voter correctly"

  assert ((Map.lookup (voter1, True) proposal2Voters) == Just 3) $ "proposal did not track upvote count for voter correctly"
  assert ((Map.lookup (voter1, False) proposal2Voters) == Just 3) $ "proposal did not track upvote count for voter correctly"


  assert (isNothing $ (Map.lookup (voter2, True) proposal1Voters)) $ "proposal did not track upvote count for voter correctly"
  assert ((Map.lookup (voter2, False) proposal1Voters) == Just 2) $ "proposal did not track upvote count for voter correctly"

  assert ((Map.lookup (voter2, True) proposal2Voters) == Just 4) $ "proposal did not track upvote count for voter correctly"
  assert (isNothing $ (Map.lookup (voter2, False) proposal2Voters)) $ "proposal did not track upvote count for voter correctly"


voteOutdatedProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
voteOutdatedProposal originateFn = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 2)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod

  -- Create sample proposal
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        , vFrom = dodOwner2
        }

  -- Advance one voting period to a voting stage.
  advanceLevel dodPeriod

  withSender dodOwner2 $ do
    call dodDao (Call @"Vote") [params]
    -- Advance two voting period to another voting stage.
    advanceLevel (2 * dodPeriod)
    call dodDao (Call @"Vote") [params]
      & expectCustomErrorNoArg #vOTING_STAGE_OVER

voteValidProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m)
  -> CheckBalanceFn m
  -> m ()
voteValidProposal originateFn checkBalanceFn = do
  DaoOriginateData{..} <- originateFn voteConfig defaultQuorumThreshold

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 2)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod

  -- Create sample proposal (first proposal has id = 0)
  key1 <- createSampleProposal 1 dodOwner1 dodDao
  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        , vFrom = dodOwner2
        }

  -- Advance one voting period to a voting stage.
  advanceLevel dodPeriod
  withSender dodOwner2 $ call dodDao (Call @"Vote") [params]
  checkBalanceFn (unTAddress dodDao) dodOwner2 2
  -- TODO [#31]: check if the vote is updated properly
  --
voteDeletedProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m)
  -> m ()
voteDeletedProposal originateFn = do
  DaoOriginateData{..} <- originateFn voteConfig defaultQuorumThreshold

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 2)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod

  -- Create sample proposal (first proposal has id = 0)
  key1 <- createSampleProposal 1 dodOwner1 dodDao
  let params = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        , vFrom = dodOwner2
        }

  -- Advance one voting period to a voting stage.
  advanceLevel dodPeriod
  withSender dodOwner1 $ call dodDao (Call @"Drop_proposal") key1
  withSender dodOwner2 $ call dodDao (Call @"Vote") [params]
    & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST

voteWithPermit
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> CheckBalanceFn m -> m ()
voteWithPermit originateFn checkBalanceFn = do
  DaoOriginateData{..} <- originateFn voteConfig defaultQuorumThreshold
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 12)

  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod

  -- Create sample proposal
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  params <- permitProtect dodOwner1 =<< addDataToSign dodDao (Nonce 0)
        VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        , vFrom = dodOwner1
        }

  -- Advance one voting period to a voting stage.
  advanceLevel dodPeriod

  withSender dodOwner2 $ call dodDao (Call @"Vote") [params]
  checkBalanceFn (unTAddress dodDao) dodOwner1 12

voteWithPermitNonce
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> GetVotePermitsCounterFn m -> m ()
voteWithPermitNonce originateFn getVotePermitsCounterFn = do

  DaoOriginateData{..} <- originateFn voteConfig defaultQuorumThreshold

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 60)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 50)

  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod

  -- Create sample proposal
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  let voteParam = VoteParam
        { vVoteType = True
        , vVoteAmount = 2
        , vProposalKey = key1
        , vFrom = dodOwner1
        }

  -- Advance one voting period to a voting stage.
  advanceLevel dodPeriod
  -- Going to try calls with different nonces
  signed1@(_          , _) <- addDataToSign dodDao (Nonce 0) voteParam
  signed2@(dataToSign2, _) <- addDataToSign dodDao (Nonce 1) voteParam
  signed3@(_          , _) <- addDataToSign dodDao (Nonce 2) voteParam

  params1 <- permitProtect dodOwner1 signed1
  params2 <- permitProtect dodOwner1 signed2
  params3 <- permitProtect dodOwner1 signed3

  withSender dodOwner2 $ do
    -- Good nonce
    call dodDao (Call @"Vote") [params1]

    -- Outdated nonce
    call dodDao (Call @"Vote") [params1]
      & expectCustomError #mISSIGNED (checkedCoerce $ lPackValue dataToSign2)

    -- Nonce from future
    call dodDao (Call @"Vote") [params3]
      & expectCustomError #mISSIGNED (checkedCoerce $ lPackValue dataToSign2)

    -- Good nonce after the previous successful entrypoint call
    call dodDao (Call @"Vote") [params2]

  -- Check counter
  (Nonce counter) <- getVotePermitsCounterFn (unTAddress dodDao)
  counter @== 2

votesBoundedValue
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
votesBoundedValue originateFn = do
  DaoOriginateData{..} <- originateFn
    ( voteConfig >>-
      ConfigDesc configConsts{ cmMaxVoters = Just 1 }
    ) defaultQuorumThreshold
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 2)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 11)

  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod
  key1 <- createSampleProposal 1 dodOwner2 dodDao
  let upvote' = NoPermit VoteParam
        { vVoteType = False
        , vVoteAmount = 1
        , vProposalKey = key1
        , vFrom = dodOwner2
        }
      downvote' = NoPermit VoteParam
        { vVoteType = False
        , vVoteAmount = 1
        , vProposalKey = key1
        , vFrom = dodOwner1
        }
  -- Advance one voting period to a voting stage.
  advanceLevel dodPeriod
  withSender dodOwner1 $ do
    call dodDao (Call @"Vote") [downvote']

  withSender dodOwner2 $ do
    call dodDao (Call @"Vote") [upvote']
      & expectCustomErrorNoArg #mAX_VOTERS_REACHED
