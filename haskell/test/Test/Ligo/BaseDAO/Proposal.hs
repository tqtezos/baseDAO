-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
{-# LANGUAGE ApplicativeDo #-}

module Test.Ligo.BaseDAO.Proposal
  ( test_BaseDAO_Proposal
  ) where

import Lorentz hiding (assert, (>>))
import Universum

import Morley.Nettest.Tasty (nettestScenarioCaps)
import Test.Tasty (TestTree, testGroup)

import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.BaseDAO.Proposal.Flush
import Test.Ligo.BaseDAO.Proposal.Propose
import Test.Ligo.BaseDAO.Proposal.Quorum
import Test.Ligo.BaseDAO.Proposal.Tokens
import Test.Ligo.BaseDAO.Proposal.Vote

test_BaseDAO_Proposal :: [TestTree]
test_BaseDAO_Proposal =
  [ testGroup "Proposal creator:"
      [ nettestScenarioCaps "BaseDAO - can propose a valid proposal" $
          validProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioCaps "cannot propose an invalid proposal (rejected)" $
          rejectProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioCaps "cannot propose a non-unique proposal" $
          nonUniqueProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioCaps "cannot propose same proposal even after dropping original one" $
          nonUniqueProposalEvenAfterDrop (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioCaps "cannot propose in a non-proposal period" $
          nonProposalPeriodProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      ]

  , testGroup "Voter:"
      [ nettestScenarioCaps "can vote on a valid proposal" $
          voteValidProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioCaps "cannot vote on a deleted proposal" $
          voteDeletedProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioCaps "cannot vote non-existing proposal" $
          voteNonExistingProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioCaps "can vote on multiple proposals" $
          voteMultiProposals (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioCaps "cannot vote on outdated proposal" $
          voteOutdatedProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioCaps "proposal track votes" $
          proposalCorrectlyTrackVotes (originateLigoDaoWithConfigDesc dynRecUnsafe)

      ]


  , nettestScenarioCaps "cannot vote if the vote amounts exceeds token balance" $
      insufficientTokenVote (originateLigoDaoWithConfigDesc dynRecUnsafe)

  -- Note: When checking storage, we need to split the test into 2 (emulator and network) as demonstrated below:
  , nettestScenarioCaps "cannot propose with insufficient tokens " $
      insufficientTokenProposal (originateLigoDaoWithConfigDesc dynRecUnsafe) (\addr -> (length . sProposalKeyListSortByDateRPC . fsStorageRPC) <$> getStorageRPC (TAddress addr))

  , testGroup "Permit:"
      [ nettestScenarioCaps "can vote from another user behalf" $
          voteWithPermit (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioCaps "counter works properly in permits" $
          voteWithPermitNonce (originateLigoDaoWithConfigDesc dynRecUnsafe)

      ]
  , testGroup "Admin:"
      [ nettestScenarioCaps "can flush proposals that got accepted" $
          flushAcceptedProposals (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioCaps "can flush 2 proposals that got accepted" $
          flushAcceptedProposalsWithAnAmount
            (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioCaps "can flush proposals that got rejected due to not meeting quorum_threshold" $
          flushRejectProposalQuorum (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioCaps "can flush proposals that got rejected due to negative votes" $
          flushRejectProposalNegativeVotes (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioCaps "flush should not affect proposals that cannot be flushed yet" $
          flushProposalFlushTimeNotReach (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioCaps "flush should fail on expired proposals" $
          flushFailOnExpiredProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioCaps "flush with bad cRejectedProposalSlashValue" $
          flushWithBadConfig (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioCaps "flush and run decision lambda" $
          flushDecisionLambda (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioCaps "empty flush calls are rejected" $
          flushNotEmpty (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioCaps "can drop proposals, only when allowed" $
          dropProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      ]

  , testGroup "Bounded Value"
      [ nettestScenarioCaps "bounded value on proposals" $
          proposalBoundedValue (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioCaps "bounded value on votes" $
          votesBoundedValue (originateLigoDaoWithConfigDesc dynRecUnsafe)

      ]

  , testGroup "Freeze-Unfreeze"
      [ nettestScenarioCaps "can freeze tokens " $
          freezeTokens (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioCaps "cannot unfreeze tokens from the same period" $
          cannotUnfreezeFromSamePeriod (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioCaps "cannot unfreeze staked tokens" $
          cannotUnfreezeStakedTokens (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioCaps "can unfreeze tokens from the previous period" $
          canUnfreezeFromPreviousPeriod (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioCaps "correctly track freeze history" $
          checkFreezeHistoryTracking (originateLigoDaoWithConfigDesc dynRecUnsafe)
      ]

 , testGroup "LIGO-specific proposal tests:"
    [ nettestScenarioCaps "can propose a valid proposal with a fixed fee" $
        validProposalWithFixedFee

    , nettestScenarioCaps "cannot propose with insufficient tokens to pay the fee"
       cannotProposeWithInsufficientTokens

    , nettestScenarioCaps "a proposer is returned a fee after the proposal succeeds" $
       proposerIsReturnedFeeAfterSucceeding

    , nettestScenarioCaps "a proposal is rejected if upvotes > downvotes and quorum threshold is not met" $
        proposalIsRejectedIfNoQuorum

    , nettestScenarioCaps "a proposal succeeds if upvotes > downvotes and quorum threshold is met" $
        proposalSucceedsIfUpVotesGtDownvotesAndQuorum

    , nettestScenarioCaps "the fee is burned if the proposal fails" $
        burnsFeeOnFailure Downvoted

    , nettestScenarioCaps "the fee is burned if the proposal doesn't meet the quorum" $
        burnsFeeOnFailure QuorumNotMet

    , nettestScenarioCaps "the frozen tokens are correctly unstaked when address cast multiple votes" $
        unstakesTokensForMultipleVotes
    ]

  , testGroup "QuorumThreshold Updates"
      [ nettestScenarioCaps "updates quorum-threshold correctly" $
          checkQuorumThresholdDynamicUpdate (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenarioCaps "updates quorum-threshold correctly within upper bounds" $
          checkQuorumThresholdDynamicUpdateUpperBound (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenarioCaps "updates quorum-threshold correctly within lower bounds" $
          checkQuorumThresholdDynamicUpdateLowerBound (originateLigoDaoWithConfigDesc dynRecUnsafe)
      , nettestScenarioCaps "proposal saves quorum for cycle" $
          checkProposalSavesQuorum (originateLigoDaoWithConfigDesc dynRecUnsafe)
      ]
  ]
