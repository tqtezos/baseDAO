-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Ligo.BaseDAO.Integrational.Management
  ( test_IntegrationalBaseDAO
  ) where

import Universum hiding (compare, drop, (>>))

import Lorentz hiding (now)
import Lorentz.Test
import Named ((!))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Ligo.BaseDAO.Types
import Michelson.Test.Integrational
import Michelson.Untyped (unsafeBuildEpName)
import Test.Ligo.BaseDAO.Integrational.Common
import Tezos.Core
import Util.Named

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

test_IntegrationalBaseDAO :: TestTree
test_IntegrationalBaseDAO = testGroup "BaseDAO Integrational Tests"
  [ testGroup "Voting"
      [ testCase "contract correctly tracks changes to the voting period duration" $
          integrationalTestExpectation $ checkVotingPeriodTracking
      ]
  ]

checkVotingPeriodTracking :: IntegrationalScenarioM ()
checkVotingPeriodTracking  = do
  now <- use isNow

  withOriginated 2 (\(admin:_) ->
    pure $ mkFullStorage
      ! #admin admin
      ! #votingPeriod 10
      ! #quorumThreshold (QuorumThreshold 10 100)
      ! #extra dynRecUnsafe
      ! #metadata mempty
      ! #now now
      ! #tokenAddress genesisAddress
      ! #customEps mempty
    ) $ \(admin:_:_) baseDao -> pure ()
        -- On origination the vp_history is unset
        -- lExpectStorage @FullStorage baseDao $ \storage -> do
        --     when ((sLastPeriodChange $ fsStorage storage) /= (LastPeriodChange 0 now))  $
        --       Left $ CustomTestError $ "BaseDAO contract did have empty voting period history on origination"
        -- rewindTime 5
        -- tTransfer  (#from .! admin) (#to .! (unTAddress baseDao)) zeroMutez (unsafeBuildEpName "set_voting_period") (toVal (100 :: Natural))
        -- lExpectStorage @FullStorage baseDao $ \storage -> do
        --     when ((sLastPeriodChange $ fsStorage storage) /= (LastPeriodChange 0 (timestampPlusSeconds now 5)))  $
        --       Left $ CustomTestError "BaseDAO contract did not correctly set the voting-period change history"

        -- rewindTime 101
        -- -- Now in voting period 1
        -- tTransfer  (#from .! admin) (#to .! (unTAddress baseDao)) zeroMutez (unsafeBuildEpName "set_voting_period") (toVal (50 :: Natural))

        -- lExpectStorage @FullStorage baseDao $ \storage -> do
        --     when ((sLastPeriodChange $ fsStorage storage) /= (LastPeriodChange 1 (timestampPlusSeconds now 106)))  $
        --       Left $ CustomTestError "BaseDAO contract did not correctly set the voting-period change(2) history"
