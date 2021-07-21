-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | Read a contract at compile time.
module Ligo.Util
  ( fetchContract
  , fetchValue
  , fetchValues
  ) where

import Universum

import Fmt (pretty)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (qAddDependentFile)
import qualified Language.Haskell.TH.Syntax as TH
import System.Directory (listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

import Lorentz.Constraints.Scopes
import Michelson.Runtime.Import (readContract, readValue)
import Michelson.Typed

-- | Read a contract at compile time assuming its expected type is known.
--
-- This is not an ideal implementation, e.g. it does not pretty-print
-- types in error messages on types mismatch.
fetchContract :: forall cp st. (KnownValue cp, KnownValue st) => String -> TH.ExpQ
fetchContract envKey = do
  path <- resolveSourcePath "haskell/test/baseDAO.tz" envKey
                          -- ↑ This default path works on CI.
                          -- There it's relative to the repo root, apparently.
  contract <- readDependentSource path

  case readContract @(ToT cp) @(ToT st) path contract of
    Left e ->
      -- Emit a compiler error if the contract cannot be read.
      fail (pretty e)
    Right _ ->
      -- Emit a haskell expression that reads the contract.
      [|
        -- Note: it's ok to use `error` here, because we just proved that the contract
        -- can be parsed+typechecked.
        either (error . pretty) id $
          readContract path contract
      |]


-- | Reads a Michelson expression into a known typed value at compile type.
fetchValue :: forall st. KnownIsoT st => FilePath -> String -> TH.ExpQ
fetchValue defaultPath envKey = do
  path <- resolveSourcePath defaultPath envKey
  valueLiteral <- readDependentSource path
  verifiedFetchedValue @st valueLiteral

-- | Reads several Michelson expression from a directory into a list of
-- known typed value at compile type.
fetchValues :: forall st. KnownIsoT st => FilePath -> String -> TH.ExpQ
fetchValues defaultPath envKey = do
  dirPath <- resolveSourcePath defaultPath envKey
  filePaths <- liftIO $ listDirectory dirPath
  valueLiterals <- forM (map (dirPath </>) filePaths) readDependentSource
  TH.listE $ map (verifiedFetchedValue @st) valueLiterals


verifiedFetchedValue :: forall st. KnownIsoT st => Text -> TH.ExpQ
verifiedFetchedValue valueLiteral =
  case readValue @(ToT st) "" valueLiteral of
    Left e ->
      -- Emit a compiler error if the value cannot be read.
      fail (pretty e)
    Right _ ->
      -- Emit a haskell expression that reads the value.
      [|
        -- Note: it's ok to use `error` here, because we just proved that the
        -- value can be parsed+typechecked.
        either (error . pretty) fromVal $ readValue "" valueLiteral
      |]

readDependentSource
  :: forall m. (MonadIO m, TH.Quasi m)
  => FilePath
  -> m Text
readDependentSource path = do
  -- We cannot use 'embedFile' directly because it returns Exp,
  -- doing the following should be equivalent
  qAddDependentFile path
  readFile path

resolveSourcePath
  :: forall m. (MonadIO m, TH.Quasi m)
  => FilePath
  -> String
  -> m FilePath
resolveSourcePath defaultPath envKey =
  fromMaybe defaultPath <$> liftIO (lookupEnv envKey)
