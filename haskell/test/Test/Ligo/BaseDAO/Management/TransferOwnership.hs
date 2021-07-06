-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
--
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- For all the incomplete list pattern matches in the calls to the
-- `withOriginated` function

module Test.Ligo.BaseDAO.Management.TransferOwnership
  ( authenticateSender
  , bypassAcceptForSelf
  , changeToPendingAdmin
  , invalidatePendingOwner
  , noPendingAdmin
  , notSetAdmin
  , pendingOwnerNotTheSame
  , rewritePendingOwner
  , transferOwnership
  , transferOwnershipSetsPendingOwner
  ) where

import Universum

import Lorentz hiding (assert, (>>))
import Morley.Nettest
import Util.Named

import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common

type WithOriginateFn m = Integer
  -> ([Address] -> FullStorage)
  -> ([Address] -> TAddress Parameter  -> m ())
  -> m ()

type WithStorage = Address -> FullStorage

transferOwnership
  :: MonadNettest caps base m
  => WithOriginateFn m -> WithStorage -> m ()
transferOwnership withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $ \[_, wallet1] baseDao ->
    withSender wallet1 $ call baseDao (Call @"Transfer_ownership") (#newOwner .! wallet1)
      & expectNotAdmin

transferOwnershipSetsPendingOwner
  :: MonadNettest caps base m
  => WithOriginateFn m -> WithStorage -> m ()
transferOwnershipSetsPendingOwner withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $ \[owner, wallet1] baseDao -> do
    withSender owner $ call baseDao (Call @"Transfer_ownership") (#newOwner .! wallet1)
    mNewPendingOwner <- (sPendingOwnerRPC . fsStorageRPC) <$> getStorageRPC baseDao
    assert (mNewPendingOwner == wallet1) "Pending owner was not set as expected"

authenticateSender
  :: MonadNettest caps base m
  => WithOriginateFn m -> WithStorage -> m ()
authenticateSender withOriginatedFn initialStorage =
  withOriginatedFn 3 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1, wallet2] baseDao -> do
      withSender owner $ call baseDao (Call @"Transfer_ownership")
        (#newOwner .! wallet1)
      withSender wallet2 $ call baseDao (Call @"Accept_ownership") ()
        & expectNotPendingOwner

changeToPendingAdmin
  :: MonadNettest caps base m
  => WithOriginateFn m -> WithStorage -> m ()
changeToPendingAdmin withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1] baseDao -> do
      withSender owner $ call baseDao (Call @"Transfer_ownership")
        (#newOwner .! wallet1)
      withSender wallet1 $ call baseDao (Call @"Accept_ownership") ()

noPendingAdmin
  :: MonadNettest caps base m
  => WithOriginateFn m -> WithStorage -> m ()
noPendingAdmin withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[_, wallet1] baseDao -> do
      withSender wallet1 $
        call baseDao (Call @"Accept_ownership") ()
        & expectNotPendingOwner

pendingOwnerNotTheSame
  :: MonadNettest caps base m
  => WithOriginateFn m -> WithStorage -> m ()
pendingOwnerNotTheSame withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1] baseDao -> withSender owner $ do
      call baseDao (Call @"Transfer_ownership")
        (#newOwner .! wallet1)
      call baseDao (Call @"Accept_ownership") ()
      & expectNotPendingOwner

notSetAdmin
  :: MonadNettest caps base m
  => WithOriginateFn m -> WithStorage -> m ()
notSetAdmin withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1] baseDao -> do
      withSender owner . inBatch $ do
        call baseDao (Call @"Transfer_ownership")
          (#newOwner .! wallet1)
        -- Make the call once again to make sure the admin still retains admin
        -- privileges
        call baseDao (Call @"Transfer_ownership")
          (#newOwner .! wallet1)
        pure ()

rewritePendingOwner
  :: MonadNettest caps base m
  => WithOriginateFn m -> WithStorage -> m ()
rewritePendingOwner withOriginatedFn initialStorage =
  withOriginatedFn 3 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1, wallet2] baseDao -> do
      withSender owner . inBatch $ do
        call baseDao (Call @"Transfer_ownership")
          (#newOwner .! wallet1)
        call baseDao (Call @"Transfer_ownership")
          (#newOwner .! wallet2)
        pure ()
      -- Make the accept ownership call from wallet1 and see that it fails
      withSender wallet1 $ call baseDao (Call @"Accept_ownership") ()
        & expectNotPendingOwner
      -- Make the accept ownership call from wallet1 and see that it works
      withSender wallet2 $ call baseDao (Call @"Accept_ownership") ()

invalidatePendingOwner
  :: MonadNettest caps base m
  => WithOriginateFn m -> WithStorage -> m ()
invalidatePendingOwner withOriginatedFn initialStorage =
  withOriginatedFn 2 (\(owner:_) -> initialStorage owner) $
    \[owner, wallet1] baseDao -> do
      withSender owner . inBatch $ do
          call baseDao (Call @"Transfer_ownership")
            (#newOwner .! wallet1)
          call baseDao (Call @"Transfer_ownership")
            (#newOwner .! owner)
          pure ()
      -- Make the accept ownership call from wallet1 and see that it fails
      -- with 'not pending owner' error
      withSender wallet1 $ call baseDao (Call @"Accept_ownership") ()
        & expectNotPendingOwner

bypassAcceptForSelf
  :: MonadNettest caps base m
  => WithOriginateFn m -> WithStorage -> m ()
bypassAcceptForSelf withOriginatedFn initialStorage =
  withOriginatedFn 1 (\(owner:_) -> initialStorage owner) $
    \[owner] baseDao -> do
      withSender owner $ do
        call baseDao (Call @"Transfer_ownership")
          (#newOwner .! (unTAddress baseDao))
      -- Same call should return error, because admin has been changed
      withSender owner $ do
        call baseDao (Call @"Transfer_ownership")
          (#newOwner .! (unTAddress baseDao))
        & expectNotAdmin
      currentAdmin <- (sAdminRPC . fsStorageRPC) <$> getStorage @FullStorage (unTAddress baseDao)
      assert (currentAdmin == (unTAddress baseDao)) "Admin address was not set"

expectNotAdmin
  :: (MonadNettest caps base m)
  => m a -> m ()
expectNotAdmin = expectCustomErrorNoArg #nOT_ADMIN

expectNotPendingOwner
  :: (MonadNettest caps base m)
  => m a -> m ()
expectNotPendingOwner = expectCustomErrorNoArg #nOT_PENDING_ADMIN
