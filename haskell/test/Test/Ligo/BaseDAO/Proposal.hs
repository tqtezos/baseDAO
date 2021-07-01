-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
{-# LANGUAGE ApplicativeDo #-}

module Test.Ligo.BaseDAO.Proposal
  ( test_BaseDAO_Proposal
  ) where

import Lorentz hiding (assert, (>>))
import Universum

import Morley.Nettest.Tasty (nettestScenarioOnEmulatorCaps, nettestScenarioOnNetworkCaps)
import Test.Tasty (TestTree, testGroup)

import Ligo.BaseDAO.Types
import Morley.Nettest
import Test.Ligo.BaseDAO.Common
import Test.Ligo.BaseDAO.Proposal.Flush
import Test.Ligo.BaseDAO.Proposal.Propose
import Test.Ligo.BaseDAO.Proposal.Quorum
import Test.Ligo.BaseDAO.Proposal.Tokens
import Test.Ligo.BaseDAO.Proposal.Vote

test_BaseDAO_Proposal :: [TestTree]
test_BaseDAO_Proposal =
  [ testGroup "Proposal creator:"
      [ nettestScenarioOnEmulatorCaps "BaseDAO - can propose a valid proposal (emulator)" $
          validProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)
            getFrozenTotalSupplyEmulator
            getFreezeHistoryEmulator

      , nettestScenarioOnEmulatorCaps "cannot propose an invalid proposal (rejected)" $
          rejectProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioOnEmulatorCaps "cannot propose a non-unique proposal" $
          nonUniqueProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioOnEmulatorCaps "cannot propose same proposal even after dropping original one" $
          nonUniqueProposalEvenAfterDrop (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioOnEmulatorCaps "cannot propose in a non-proposal period" $
          nonProposalPeriodProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      ]

  , testGroup "Voter:"
      [ nettestScenarioOnEmulatorCaps "can vote on a valid proposal" $
          voteValidProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)
            checkBalanceEmulator

      , nettestScenarioOnEmulatorCaps "cannot vote on a deleted proposal" $
          voteDeletedProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioOnEmulatorCaps "cannot vote non-existing proposal" $
          voteNonExistingProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioOnEmulatorCaps "can vote on multiple proposals" $
          voteMultiProposals (originateLigoDaoWithConfigDesc dynRecUnsafe)
            checkBalanceEmulator

      , nettestScenarioOnEmulatorCaps "cannot vote on outdated proposal" $
          voteOutdatedProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioOnEmulatorCaps "proposal track votes" $
          proposalCorrectlyTrackVotes (originateLigoDaoWithConfigDesc dynRecUnsafe) getProposalEmulator

      ]


  , nettestScenarioOnEmulatorCaps "cannot vote if the vote amounts exceeds token balance" $
      insufficientTokenVote (originateLigoDaoWithConfigDesc dynRecUnsafe)

  -- Note: When checking storage, we need to split the test into 2 (emulator and network) as demonstrated below:
  , nettestScenarioOnEmulatorCaps "cannot propose with insufficient tokens (emulator) " $
      insufficientTokenProposal (originateLigoDaoWithConfigDesc dynRecUnsafe) (\addr -> (length . sProposalKeyListSortByDate . fsStorage) <$> getFullStorage @FullStorage addr)

  , nettestScenarioOnNetworkCaps "cannot propose with insufficient tokens (network) " $
      insufficientTokenProposal (originateLigoDaoWithConfigDesc dynRecUnsafe) (\addr -> (length . sProposalKeyListSortByDateRPC . fsStorageRPC) <$> getStorage @FullStorage addr)

  , testGroup "Permit:"
      [ nettestScenarioOnEmulatorCaps "can vote from another user behalf" $
          voteWithPermit (originateLigoDaoWithConfigDesc dynRecUnsafe)
            checkBalanceEmulator

      , nettestScenarioOnEmulatorCaps "counter works properly in permits" $
          voteWithPermitNonce (originateLigoDaoWithConfigDesc dynRecUnsafe) getVotePermitsCounterEmulator

      ]
  , testGroup "Admin:"
      [ nettestScenarioOnEmulatorCaps "can flush proposals that got accepted" $
          flushAcceptedProposals (originateLigoDaoWithConfigDesc dynRecUnsafe) getFreezeHistoryEmulator

      , nettestScenarioOnEmulatorCaps "can flush 2 proposals that got accepted" $
          flushAcceptedProposalsWithAnAmount
            (originateLigoDaoWithConfigDesc dynRecUnsafe)
            checkBalanceEmulator

      , nettestScenarioOnEmulatorCaps "can flush proposals that got rejected due to not meeting quorum_threshold" $
          flushRejectProposalQuorum (originateLigoDaoWithConfigDesc dynRecUnsafe)
            checkBalanceEmulator

      , nettestScenarioOnEmulatorCaps "can flush proposals that got rejected due to negative votes" $
          flushRejectProposalNegativeVotes (originateLigoDaoWithConfigDesc dynRecUnsafe)
            checkBalanceEmulator

      , nettestScenarioOnEmulatorCaps "flush should not affect proposals that cannot be flushed yet" $
          flushProposalFlushTimeNotReach (originateLigoDaoWithConfigDesc dynRecUnsafe)
            checkBalanceEmulator

      , nettestScenarioOnEmulatorCaps "flush should fail on expired proposals" $
          flushFailOnExpiredProposal (originateLigoDaoWithConfigDesc dynRecUnsafe)
            checkBalanceEmulator

      , nettestScenarioOnEmulatorCaps "flush with bad cRejectedProposalSlashValue" $
          flushWithBadConfig (originateLigoDaoWithConfigDesc dynRecUnsafe)
            checkBalanceEmulator

      , nettestScenarioOnEmulatorCaps "flush and run decision lambda" $
          flushDecisionLambda (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioOnEmulatorCaps "empty flush calls are rejected" $
          flushNotEmpty (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioOnEmulatorCaps "can drop proposals, only when allowed" $
          dropProposal (originateLigoDaoWithConfigDesc dynRecUnsafe) checkBalanceEmulator

      ]

  , testGroup "Bounded Value"
      [ nettestScenarioOnEmulatorCaps "bounded value on proposals" $
          proposalBoundedValue (originateLigoDaoWithConfigDesc dynRecUnsafe)

      , nettestScenarioOnEmulatorCaps "bounded value on votes" $
          votesBoundedValue (originateLigoDaoWithConfigDesc dynRecUnsafe)

      ]

  , testGroup "Freeze-Unfreeze"
      [ nettestScenarioOnNetworkCaps "can freeze tokens (emulator) " $
          freezeTokens (originateLigoDaoWithConfigDesc dynRecUnsafe)
            (\_ _ _  -> pure ())
      , nettestScenarioOnEmulatorCaps "can freeze tokens (network) " $
          freezeTokens (originateLigoDaoWithConfigDesc dynRecUnsafe)
            checkBalanceEmulator

      , nettestScenarioOnEmulatorCaps "cannot unfreeze tokens from the same period" $
          cannotUnfreezeFromSamePeriod (originateLigoDaoWithConfigDesc dynRecUnsafe)
            checkBalanceEmulator

      , nettestScenarioOnEmulatorCaps "cannot unfreeze staked tokens" $
          cannotUnfreezeStakedTokens (originateLigoDaoWithConfigDesc dynRecUnsafe)
            checkBalanceEmulator

      , nettestScenarioOnEmulatorCaps "can unfreeze tokens from the previous period" $
          canUnfreezeFromPreviousPeriod (originateLigoDaoWithConfigDesc dynRecUnsafe)
            checkBalanceEmulator

      , nettestScenarioOnEmulatorCaps "correctly track freeze history" $
          checkFreezeHistoryTracking (originateLigoDaoWithConfigDesc dynRecUnsafe)
            getFreezeHistoryEmulator
      ]

 , testGroup "LIGO-specific proposal tests:"
    [ nettestScenarioOnEmulatorCaps "can propose a valid proposal with a fixed fee" $
        validProposalWithFixedFee getFrozenTotalSupplyEmulator getFreezeHistoryEmulator

    , nettestScenarioOnEmulatorCaps "cannot propose with insufficient tokens to pay the fee"
       cannotProposeWithInsufficientTokens

    , nettestScenarioOnEmulatorCaps "a proposer is returned a fee after the proposal succeeds" $
       proposerIsReturnedFeeAfterSucceeding checkBalanceEmulator

    , nettestScenarioOnEmulatorCaps "a proposal is rejected if upvotes > downvotes and quorum threshold is not met" $
        proposalIsRejectedIfNoQuorum checkBalanceEmulator

    , nettestScenarioOnEmulatorCaps "a proposal succeeds if upvotes > downvotes and quorum threshold is met" $
        proposalSucceedsIfUpVotesGtDownvotesAndQuorum checkBalanceEmulator

    , nettestScenarioOnEmulatorCaps "the fee is burned if the proposal fails" $
        burnsFeeOnFailure Downvoted checkBalanceEmulator

    , nettestScenarioOnEmulatorCaps "the fee is burned if the proposal doesn't meet the quorum" $
        burnsFeeOnFailure QuorumNotMet checkBalanceEmulator

    , nettestScenarioOnEmulatorCaps "the frozen tokens are correctly unstaked when address cast multiple votes" $
        unstakesTokensForMultipleVotes getFreezeHistoryEmulator
    ]

  , testGroup "QuorumThreshold Updates"
      [ nettestScenarioOnEmulatorCaps "updates quorum-threshold correctly" $
          checkQuorumThresholdDynamicUpdate (originateLigoDaoWithConfigDesc dynRecUnsafe)
            getQtAtCycleEmulator
      , nettestScenarioOnEmulatorCaps "updates quorum-threshold correctly within upper bounds" $
          checkQuorumThresholdDynamicUpdateUpperBound (originateLigoDaoWithConfigDesc dynRecUnsafe)
            getQtAtCycleEmulator
      , nettestScenarioOnEmulatorCaps "updates quorum-threshold correctly within lower bounds" $
          checkQuorumThresholdDynamicUpdateLowerBound (originateLigoDaoWithConfigDesc dynRecUnsafe)
            getQtAtCycleEmulator
      , nettestScenarioOnEmulatorCaps "proposal saves quorum for cycle" $
          checkProposalSavesQuorum (originateLigoDaoWithConfigDesc dynRecUnsafe)
            getProposalEmulator
      ]
  ]
