-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
--
{-# LANGUAGE ApplicativeDo #-}

-- | Contains tests on @flush@ entrypoint logic.
module Test.Ligo.BaseDAO.Proposal.Flush
  ( flushProposalFlushTimeNotReach
  , flushAcceptedProposalsWithAnAmount
  , flushAcceptedProposals
  , flushRejectProposalQuorum
  , flushRejectProposalNegativeVotes
  , flushFailOnExpiredProposal
  , flushWithBadConfig
  , flushDecisionLambda
  , flushNotEmpty
  ) where

import Universum

import Lorentz hiding (assert, (>>))
import Lorentz.Test (contractConsumer)
import Morley.Nettest
import Util.Named

import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.BaseDAO.Proposal.Config

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

flushAcceptedProposals
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> GetFreezeHistoryFn m -> m ()
flushAcceptedProposals originateFn getFreezeHistoryFn = do
-- Use 60s for voting period, since in real network by the time we call
  -- vote entrypoint 30s is already passed.
  DaoOriginateData{..} <- originateFn (testConfig
      >>- (ConfigDesc $ Period 60)
      >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 120 })
      >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 180 })
      ) defaultQuorumThreshold

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 15)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod

  -- Accepted Proposals
  key1 <- createSampleProposal 1 dodOwner1 dodDao
  -- Advance one voting period to a voting stage.
  advanceLevel dodPeriod

  let upvote' = NoPermit VoteParam
        { vFrom = dodOwner2
        , vVoteType = True
        , vVoteAmount = 10
        , vProposalKey = key1
        }
      downvote' = NoPermit VoteParam
        { vFrom = dodOwner2
        , vVoteType = False
        , vVoteAmount = 5
        , vProposalKey = key1
        }
  withSender dodOwner2 $
    call dodDao (Call @"Vote") [upvote', downvote']

  -- Checking freezing histories of proposer and voters
  fhOwner1 <- getFreezeHistoryFn (unTAddress dodDao) dodOwner1
  fhOwner1 @== Just (AddressFreezeHistory 0 0 1 10)
  fhOwner2 <- getFreezeHistoryFn (unTAddress dodDao) dodOwner2
  fhOwner2 @== Just (AddressFreezeHistory 0 0 2 15)

  -- Advance one voting period to a proposing stage.
  advanceLevel (dodPeriod+1)
  withSender dodAdmin $ call dodDao (Call @"Flush") 100

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dodDao
  --   & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST dodDao


  fhOwner1' <- getFreezeHistoryFn (unTAddress dodDao) dodOwner1
  fhOwner1' @== Just (AddressFreezeHistory 0 10 3 0)
  fhOwner2' <- getFreezeHistoryFn (unTAddress dodDao) dodOwner2
  fhOwner2' @== Just (AddressFreezeHistory 0 15 3 0)

flushAcceptedProposalsWithAnAmount
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m)
  -> CheckBalanceFn m
  -> m ()
flushAcceptedProposalsWithAnAmount originateFn checkBalanceFn = do
  DaoOriginateData{..}
    <- originateFn (testConfig
        >>- (ConfigDesc $ Period 20)
        >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 40 })
        >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 60 })
        ) defaultQuorumThreshold

  -- [Voting]
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 30)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  advanceLevel dodPeriod

  -- [Proposing]
  (key1, key2) <- createSampleProposals (1, 2) dodOwner1 dodDao
  advanceLevel 1
  _key3 <- createSampleProposal 3 dodOwner1 dodDao

  let vote' key = NoPermit VoteParam
        { vFrom = dodOwner2
        , vVoteType = True
        , vVoteAmount = 5
        , vProposalKey = key
        }

  advanceLevel dodPeriod

  -- [Voting]
  withSender dodOwner2 . inBatch $ do
      call dodDao (Call @"Vote") [vote' key1]
      call dodDao (Call @"Vote") [vote' key2]
      pure ()

  advanceLevel (dodPeriod + 1)

  -- [Proposing]
  withSender dodAdmin $ call dodDao (Call @"Flush") 2

  -- key1 and key2 are flushed. (Tokens remain the same, because they are all passed)
  checkBalanceFn (unTAddress dodDao) dodOwner1 30

  withSender dodAdmin $ call dodDao (Call @"Flush") 1

  -- key3 is rejected
  checkBalanceFn (unTAddress dodDao) dodOwner1 25

flushRejectProposalQuorum
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m)
  -> CheckBalanceFn m
  -> m ()
flushRejectProposalQuorum originateFn checkBalanceFn = do
  DaoOriginateData{..}
    <- originateFn (testConfig
        >>- (ConfigDesc $ Period 20)
        >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 40 })
        >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 60 })
        ) (mkQuorumThreshold 3 5)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 5)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod

  -- Rejected Proposal
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  let votes = fmap NoPermit
        [ VoteParam
          { vVoteType = True
          , vVoteAmount = 3
          , vProposalKey = key1
          , vFrom = dodOwner2
          }
        ]
  -- Advance one voting period to a voting stage.
  advanceLevel dodPeriod
  withSender dodOwner2 $ call dodDao (Call @"Vote") votes

  -- Advance one voting period to a proposing stage.
  advanceLevel (dodPeriod + 1)
  withSender dodAdmin $ call dodDao (Call @"Flush") 100

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dodDao
  --   & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST dodDao

  checkBalanceFn (unTAddress dodDao) dodOwner1 05
  checkBalanceFn (unTAddress dodDao) dodOwner2 05 -- Since voter tokens are not burned

flushRejectProposalNegativeVotes
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m)
  -> CheckBalanceFn m
  -> m ()
flushRejectProposalNegativeVotes originateFn checkBalanceFn = do
  DaoOriginateData{..}
    <- originateFn (testConfig
          >>- (ConfigDesc (Period 20))
          >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 40 })
          >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 60 })
          ) (mkQuorumThreshold 3 100)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 3)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod

  -- Rejected Proposal
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  let votes = fmap NoPermit
        [ VoteParam
          { vVoteType = True
          , vVoteAmount = 1
          , vProposalKey = key1
          , vFrom = dodOwner2
          }
        , VoteParam
          { vVoteType = False
          , vVoteAmount = 1
          , vProposalKey = key1
          , vFrom = dodOwner2
          }
        , VoteParam
          { vVoteType = False
          , vVoteAmount = 1
          , vProposalKey = key1
          , vFrom = dodOwner2
          }
        ]
  -- Advance one voting period to a voting stage.
  advanceLevel dodPeriod
  withSender dodOwner2 $ call dodDao (Call @"Vote") votes

  -- Check proposer balance
  checkBalanceFn (unTAddress dodDao) dodOwner1 10

  -- Advance one voting period to a proposing stage.
  advanceLevel (dodPeriod + 1)
  withSender dodAdmin $ call dodDao (Call @"Flush") 100

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dodDao
  --   & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST dodDao

  checkBalanceFn (unTAddress dodDao) dodOwner1 05
  checkBalanceFn (unTAddress dodDao) dodOwner2 03

flushWithBadConfig
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m)
  -> CheckBalanceFn m
  -> m ()
flushWithBadConfig originateFn checkBalanceFn = do
  DaoOriginateData{..} <-
    originateFn (badRejectedValueConfig
      >>- (ConfigDesc (Period 20))
      >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 40 })
      >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 60 })
      ) (mkQuorumThreshold 1 2)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 3)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  let upvote' = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 1
        , vProposalKey = key1
        , vFrom = dodOwner2
        }
  -- Advance one voting period to a voting stage.
  advanceLevel dodPeriod
  withSender dodOwner2 $ call dodDao (Call @"Vote") [upvote']

  -- Advance one voting period to a proposing stage.
  advanceLevel (dodPeriod+1)
  withSender dodAdmin $ call dodDao (Call @"Flush") 100

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dodDao
  --   & expectCustomErrorNoArg #pROPOSAL_NOT_EXIST dodDao

  checkBalanceFn (unTAddress dodDao) dodOwner1 0
  checkBalanceFn (unTAddress dodDao) dodOwner2 3

flushDecisionLambda
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
flushDecisionLambda originateFn = do
  consumer <- chAddress <$> originateSimple @("proposer" :! Address) "consumer" [] contractConsumer
  DaoOriginateData{..} <-
    originateFn ((decisionLambdaConfig (TAddress consumer))
      >>- (ConfigDesc $ Period 60)
      >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 120 })
      >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 180 })
      ) defaultQuorumThreshold

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 10)
  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 10)

  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  let upvote' = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 10
        , vProposalKey = key1
        , vFrom = dodOwner2
        }
  -- Advance one voting period to a voting stage.
  advanceLevel dodPeriod
  withSender dodOwner2 $ call dodDao (Call @"Vote") [upvote']

  -- Advance one voting period to a proposing stage.
  advanceLevel (dodPeriod + 1)
  withSender dodAdmin $ call dodDao (Call @"Flush") 100

  results <- getStorage @(["proposer" :! Address]) (toAddress consumer)
  assert (results == (#proposer <.!> [dodOwner1]))
    "Unexpected accepted proposals list"

flushFailOnExpiredProposal
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m)
  -> CheckBalanceFn m
  -> m ()
flushFailOnExpiredProposal originateFn checkBalanceFn = withFrozenCallStack $ do
  DaoOriginateData{..} <-
    originateFn
     (testConfig
       >>- (ConfigDesc (Period 20))
       >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 40 })
       >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 60 })
      ) (mkQuorumThreshold 1 50)

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 20)

  withSender dodOwner2 $
    call dodDao (Call @"Freeze") (#amount .! 20)

  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  -- Advance one voting period to a voting stage.
  advanceLevel dodPeriod
  let params key = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 20
        , vProposalKey = key
        , vFrom = dodOwner2
        }
  withSender dodOwner2 $ call dodDao (Call @"Vote") [params key1]
  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod
  _key2 <- createSampleProposal 2 dodOwner1 dodDao

  advanceLevel (2*dodPeriod + 1)
  -- `key1` is now expired, and `key2` is not yet expired.
  withSender dodAdmin $ call dodDao (Call @"Flush") 2
    & expectCustomErrorNoArg #eXPIRED_PROPOSAL

  -- `key1` is expired, so it is possible to `drop_proposal`
  withSender dodOwner2 $ do
    call dodDao (Call @"Drop_proposal") key1

  withSender dodAdmin $ call dodDao (Call @"Flush") 1
  checkBalanceFn (unTAddress dodDao) dodOwner1 10

flushProposalFlushTimeNotReach
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m)
  -> CheckBalanceFn m
  -> m ()
flushProposalFlushTimeNotReach originateFn checkBalanceFn = do
  DaoOriginateData{..} <-
    originateFn (testConfig
        >>- (ConfigDesc $ Period 20)
        >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 40 })
        >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 60 })
        ) defaultQuorumThreshold

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 30)

  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod

  (_key1, _key2) <- createSampleProposals (1, 2) dodOwner1 dodDao
  -- Advance two voting period to another proposing stage.
  advanceLevel dodPeriod -- skip voting period
  advanceLevel (dodPeriod + 1)
  _key3 <- createSampleProposal 3 dodOwner1 dodDao

  withSender dodAdmin $ call dodDao (Call @"Flush") 100
  checkBalanceFn (unTAddress dodDao) dodOwner1 (5 + 5 + 10) -- first 2 proposals got flushed then slashed by 5, the last one is not affected.

  -- TODO: [#31]
  -- checkIfAProposalExist (key1 :: ByteString) dodDao
  -- checkIfAProposalExist (key2 :: ByteString) dodDao


flushNotEmpty
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m) -> m ()
flushNotEmpty originateFn = withFrozenCallStack $ do
  DaoOriginateData{..} <-
    originateFn
     (testConfig
       >>- (ConfigDesc (Period 20))
       >>- (ConfigDesc configConsts{ cmProposalFlushTime = Just 40 })
       >>- (ConfigDesc configConsts{ cmProposalExpiredTime = Just 60 })
      ) (mkQuorumThreshold 1 50)

  -- no proposal exist at this point, so flush is empty
  withSender dodAdmin $ call dodDao (Call @"Flush") 1
    & expectCustomErrorNoArg #eMPTY_FLUSH

  withSender dodOwner1 $ call dodDao (Call @"Freeze") (#amount .! 20)
  withSender dodOwner2 $ call dodDao (Call @"Freeze") (#amount .! 20)

  -- Advance one voting period to a proposing stage.
  advanceLevel dodPeriod
  key1 <- createSampleProposal 1 dodOwner1 dodDao

  -- Advance one voting period to a voting stage.
  advanceLevel dodPeriod
  let params key = NoPermit VoteParam
        { vVoteType = True
        , vVoteAmount = 20
        , vProposalKey = key
        , vFrom = dodOwner2
        }
  withSender dodOwner2 $ call dodDao (Call @"Vote") [params key1]

  -- Advance one voting period to a proposing stage.
  advanceLevel (dodPeriod - 1)
  -- the proposal exists at this point (and has votes), but it can't be flushed
  -- yet, because it needs one more level to meet the `proposal_flush_time`
  withSender dodAdmin $ call dodDao (Call @"Flush") 1
    & expectCustomErrorNoArg #eMPTY_FLUSH

  -- however after one more level flushing is allowed
  advanceLevel 1
  withSender dodAdmin $ call dodDao (Call @"Flush") 1
