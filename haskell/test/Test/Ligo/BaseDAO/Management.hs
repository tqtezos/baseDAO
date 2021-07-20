-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
--
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- For all the incomplete list pattern matches in the calls to the
-- `withOriginated` function

module Test.Ligo.BaseDAO.Management
  ( test_BaseDAO_Management
  ) where

import Universum

import Named (defaults, (!))
import Test.Tasty (TestTree, testGroup)

import Lorentz as L hiding (now, (>>))
import Michelson.Runtime.GState (genesisAddress)
import Michelson.Typed (convertContract)
import Michelson.Typed.Convert (untypeValue)
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)

import Ligo.BaseDAO.Contract
import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Management.TransferOwnership

-- | Function that originates the contract and also make a bunch of
-- address (the `addrCount` arg determines the count) for use within
-- the tests. It is not pretty, but IMO it makes the test a bit less
-- verbose.
withOriginated
  :: MonadNettest caps base m
  => Integer
  -> ([Address] -> FullStorage)
  -> ([Address] -> TAddress Parameter -> m a)
  -> m a
withOriginated addrCount storageFn tests = do
  addresses <- mapM (\x -> newAddress $ fromString ("address" <> (show x))) [1 ..addrCount]
  baseDao <- originateUntyped $ UntypedOriginateData
    { uodName = "BaseDAO Test Contract"
    , uodBalance = zeroMutez
    , uodStorage = untypeValue $ toVal $ storageFn addresses
    , uodContract = convertContract baseDAOContractLigo
    }

  tests addresses (TAddress baseDao)

-- | We test non-token entrypoints of the BaseDAO contract here
test_BaseDAO_Management :: [TestTree]
test_BaseDAO_Management =
  [ testGroup "Ownership transfer"
    [ nettestScenarioCaps "transfer ownership entrypoint authenticates sender" $ do
        current_level <- getLevel
        transferOwnership withOriginated (initialStorage current_level)

    , nettestScenarioCaps "sets pending owner" $ do
        current_level <- getLevel
        transferOwnershipSetsPendingOwner withOriginated (initialStorage current_level)

    , nettestScenarioCaps "does not set administrator" $ do
        current_level <- getLevel
        notSetAdmin withOriginated (initialStorage current_level)

    , nettestScenarioCaps "rewrite existing pending owner" $ do
        current_level <- getLevel
        rewritePendingOwner withOriginated (initialStorage current_level)

    , nettestScenarioCaps "invalidates pending owner if new owner is current admin" $ do
        current_level <- getLevel
        invalidatePendingOwner withOriginated (initialStorage current_level)

    , nettestScenarioCaps "bypasses accept_entrypoint if new admin address is self" $ do
        current_level <- getLevel
        bypassAcceptForSelf withOriginated (initialStorage current_level)
    ]
    , testGroup "Accept Ownership"
      [ nettestScenarioCaps "authenticates the sender" $ do
          current_level <- getLevel
          authenticateSender withOriginated (initialStorage current_level)

      , nettestScenarioCaps "changes the administrator to pending owner" $ do
          current_level <- getLevel
          changeToPendingAdmin withOriginated (initialStorage current_level)

      , nettestScenarioCaps "throws error when there is no pending owner" $ do
          current_level <- getLevel
          noPendingAdmin withOriginated (initialStorage current_level)

      , nettestScenarioCaps "throws error when called by current admin, when pending owner is not the same" $ do
          current_level <- getLevel
          pendingOwnerNotTheSame withOriginated (initialStorage current_level)
      ]
  ]

  where
    testCustomEntrypoint :: ('[(ByteString, FullStorage)] :-> '[([Operation], Storage)])
    testCustomEntrypoint =
      -- Unpack an address from packed bytes and set it as admin
      L.unpair #
      L.unpackRaw @Address #
      L.ifNone
        (L.unit # L.failWith)
        (L.dip (L.toField #fsStorage) # setField #sAdmin) #
      L.nil # pair

    initialStorage currentLevel admin = mkFullStorage
      ! #admin admin
      ! #extra dynRecUnsafe
      ! #metadata mempty
      ! #level currentLevel
      ! #tokenAddress genesisAddress
      ! #customEps
          [ ([mt|testCustomEp|], lPackValueRaw testCustomEntrypoint)
          ]
      ! defaults
