-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

-- | LIGO version of the contract.
module Ligo.BaseDAO.Contract
  ( baseDAOContractLigo
  ) where

import Michelson.Typed

import Ligo.BaseDAO.Types
import Ligo.Util

baseDAOContractLigo :: Contract (ToT Parameter) (ToT FullStorage)
baseDAOContractLigo =
  $(fetchContract @(ToT Parameter) @(ToT FullStorage) "BASEDAO_LIGO_PATH")
