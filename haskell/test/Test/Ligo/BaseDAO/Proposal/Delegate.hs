-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.Proposal.Delegate
  ( test_UpdateDelegates
  ) where

import Universum

import Lorentz hiding (assert, (>>))
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)
import Test.Tasty (TestTree, testGroup)
import Util.Named

import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.BaseDAO.Proposal.Config

test_UpdateDelegates :: TestTree
test_UpdateDelegates =
  testGroup "Update_delegates:"
    [ nettestScenarioCaps "add/remove a delgate" $
        addRemoveDelegate (originateLigoDaoWithConfigDesc dynRecUnsafe)
    , nettestScenarioCaps "update multiple delegates" $
        updateDelegates (originateLigoDaoWithConfigDesc dynRecUnsafe) checkIfDelegateExists
    ]

addRemoveDelegate
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m)
  -> m ()
addRemoveDelegate originateFn = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold
  let param enable = DelegateParam
        { dpEnable = enable
        , dpDelegate = dodOperator1
        }

  withSender dodOwner1 $
    call dodDao (Call @"Update_delegate") [param True]

  withSender dodOwner1 $
    call dodDao (Call @"Freeze") (#amount .! 100)

  advanceLevel dodPeriod
  let params counter = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = lPackValueRaw @Integer counter
        , ppFrom = dodOwner1
        }
      key = makeProposalKey (params 1)
      vote = NoPermit VoteParam
                { vVoteType = True
                , vVoteAmount = 1
                , vProposalKey = key
                , vFrom = dodOwner1
                }
  withSender dodOperator1 $ call dodDao (Call @"Propose") (params 1)
  advanceLevel dodPeriod
  withSender dodOperator1 $ call dodDao (Call @"Vote") [vote]

  withSender dodOwner1 $
    call dodDao (Call @"Update_delegate") [param False]

  withSender dodOperator1 $ call dodDao (Call @"Vote") [vote]
    & expectCustomErrorNoArg #nOT_DELEGATE
  advanceLevel dodPeriod
  withSender dodOperator1 $ call dodDao (Call @"Propose") (params 2)
    & expectCustomErrorNoArg #nOT_DELEGATE


updateDelegates
  :: (MonadNettest caps base m, HasCallStack)
  => (ConfigDesc Config -> OriginateFn m)
  -> (TAddress Parameter -> Delegate -> m Bool)
  -> m ()
updateDelegates originateFn isDelegateFn = do
  DaoOriginateData{..} <- originateFn testConfig defaultQuorumThreshold
  let mkDelegate addr enable = DelegateParam
        { dpEnable = enable
        , dpDelegate = addr
        }

  -- Before Delegate set
  isDelegateFn dodDao (Delegate dodOwner1 dodOperator1) @@== False
  isDelegateFn dodDao (Delegate dodOwner1 dodOperator2) @@== False

  -- Add a Delegate
  withSender dodOwner1 $
    call dodDao (Call @"Update_delegate") [mkDelegate dodOperator1 True]

  isDelegateFn dodDao (Delegate dodOwner1 dodOperator1) @@== True
  isDelegateFn dodDao (Delegate dodOwner1 dodOperator2) @@== False

  -- Add a new Delegate, remove an old one
  withSender dodOwner1 $
    call dodDao (Call @"Update_delegate")
      [mkDelegate dodOperator1 False, mkDelegate dodOperator2 True]

  isDelegateFn dodDao (Delegate dodOwner1 dodOperator1) @@== False
  isDelegateFn dodDao (Delegate dodOwner1 dodOperator2) @@== True


  -- Removing a non-existent delegate is allowed
  someAddr :: Address <- newAddress "some_address"
  withSender dodOwner1 $
    call dodDao (Call @"Update_delegate") [mkDelegate someAddr False]

  isDelegateFn dodDao (Delegate dodOwner1 someAddr) @@== False
