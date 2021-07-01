-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NumericUnderscores #-}

module Test.Ligo.BaseDAO.Common
  ( DaoOriginateData(..)
  , OriginateFn

  , dummyFA2Contract
  , makeProposalKey
  , addDataToSign
  , permitProtect
  , sendXtz

  , checkStorage
  , createSampleProposal
  , createSampleProposals
  , defaultQuorumThreshold
  , originateLigoDaoWithConfig
  , originateLigoDaoWithConfigDesc
  , originateLigoDao

  -- * Re-export
  , module Errors
  , module StorageHelper
  ) where

import Universum hiding (drop, swap)

import Lorentz hiding (assert, now, (>>))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Michelson.Typed.Convert (convertContract, untypeValue)
import Morley.Nettest
import Morley.Nettest.Caps (MonadOps)
import Named ((!))

import Ligo.BaseDAO.Contract
import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common.Errors as Errors
import Test.Ligo.BaseDAO.Common.StorageHelper as StorageHelper
import Test.Ligo.BaseDAO.Proposal.Config (ConfigDesc, fillConfig)

type OriginateFn m = QuorumThreshold -> m DaoOriginateData

type instance AsRPC FA2.TransferItem = FA2.TransferItem

data DaoOriginateData = DaoOriginateData
  { dodOwner1 :: Address
  , dodOperator1 :: Address
  , dodOwner2 :: Address
  , dodOperator2 :: Address
  , dodDao :: TAddress Parameter
  , dodTokenContract :: TAddress FA2.Parameter
  , dodAdmin :: Address
  , dodGuardian :: TAddress (Address, ProposalKey)
  , dodPeriod :: Natural
  }

-- | A dummy contract with FA2 parameter that remembers the
-- transfer calls.
dummyFA2Contract :: Contract FA2.Parameter [FA2.TransferParams]
dummyFA2Contract = defaultContract $
  unpair #
    (entryCase @FA2.Parameter (Proxy @PlainEntrypointsKind)
      ( #cBalance_of /-> Lorentz.drop
      , #cTransfer /-> cons
      , #cUpdate_operators /-> Lorentz.drop
      )) # nil # pair

-- | A dummy contract that act as guardian contract for BaseDAO
dummyGuardianContract :: Contract (Address, ProposalKey) ()
dummyGuardianContract = defaultContract $
  unpair # unpair #
  stackType @'[Address, ProposalKey, ()] #
  contractCalling @Parameter (Call @"Drop_proposal") #
  ifSome (
      swap #
      push zeroMutez # swap #
      transferTokens # nil # swap # cons
    ) (drop # nil) #
  stackType @'[[Operation], ()] #
  pair

makeProposalKey :: ProposeParams -> ProposalKey
makeProposalKey params = toHashHs $ lPackValue params

addDataToSign
  :: (MonadNettest caps base m)
  => TAddress param
  -> Nonce
  -> d
  -> m (DataToSign d, d)
addDataToSign (toAddress -> dsContract) dsNonce dsData = do
  dsChainId <- getChainId
  return (DataToSign{..}, dsData)

-- | Add a permit from given user.
permitProtect
  :: (MonadNettest caps base m, NicePackedValue a)
  => Address -> (DataToSign a, a) -> m (PermitProtected a)
permitProtect author (toSign, a) = do
  pKey <- getPublicKey author
  pSignature <- signBinary (lPackValue toSign) author
  return PermitProtected
    { ppArgument = a
    , ppPermit = Just Permit{..}
    }

sendXtz
  :: (MonadNettest caps base m, HasCallStack, NiceParameter pm)
  => Address -> EpName -> pm -> m ()
sendXtz addr epName pm = withFrozenCallStack $ do
  let transferData = TransferData
        { tdTo = addr
        , tdAmount = toMutez 0.5_e6 -- 0.5 xtz
        , tdEntrypoint = epName
        , tdParameter = pm
        }
  transfer transferData


-- TODO: Implement this via [#31] instead
-- checkIfAProposalExist
--   :: MonadNettest caps base m
--   => ProposalKey -> TAddress (Parameter TestProposalMetadata) -> m ()
-- checkIfAProposalExist proposalKey dodDao = do
--   owner :: Address <- newAddress "owner"
--   consumer <- originateSimple "consumer" [] contractConsumer
--   -- | If the proposal exists, there should be no error
--   callFrom owner dodDao (Call @"Proposal_metadata") (mkView proposalKey consumer)

-- TODO [#31]: See this ISSUES: https://gitlab.com/morley-framework/morley/-/issues/415#note_435327096
-- Check if certain field in storage
-- checkPropertyOfProposal :: _
-- checkPropertyOfProposal = error "undefined"

defaultQuorumThreshold :: QuorumThreshold
defaultQuorumThreshold = mkQuorumThreshold 1 100

originateLigoDaoWithConfig
 :: MonadNettest caps base m
 => ContractExtra
 -> Config
 -> OriginateFn m
originateLigoDaoWithConfig extra config qt = do
  owner1 :: Address <- newAddress "owner1"
  operator1 :: Address <- newAddress "operator1"
  owner2 :: Address <- newAddress "owner2"
  operator2 :: Address <- newAddress "operator2"

  admin :: Address <- newAddress "admin"

  currentLevel <- getLevel
  tokenContract <- chAddress <$> originateSimple "TokenContract" [] dummyFA2Contract
  guardianContract <- chAddress <$> originateSimple "guardian" () dummyGuardianContract

  let fullStorage = FullStorage
        { fsStorage =
            ( mkStorage
              ! #extra extra
              ! #admin admin
              ! #metadata mempty
              ! #tokenAddress tokenContract
              ! #level currentLevel
              ! #quorumThreshold qt
            )
            { sGuardian = guardianContract
            }
        , fsConfig = config
        }
  let
    originateData = UntypedOriginateData
      { uodName = "BaseDAO"
      , uodBalance = toMutez 0
      , uodStorage = untypeValue $ toVal $ fullStorage
      , uodContract = convertContract baseDAOContractLigo
      }

  daoUntyped <- originateUntyped originateData

  let dao = TAddress @Parameter daoUntyped

  pure $ DaoOriginateData owner1 operator1 owner2 operator2 dao (TAddress tokenContract)
      admin (TAddress guardianContract) (unPeriod $ cPeriod config)

originateLigoDaoWithConfigDesc
 :: MonadNettest caps base m
 => ContractExtra
 -> ConfigDesc Config
 -> OriginateFn m
originateLigoDaoWithConfigDesc extra config =
  originateLigoDaoWithConfig extra (fillConfig config defaultConfig)

originateLigoDao :: MonadNettest caps base m => OriginateFn m
originateLigoDao =
  originateLigoDaoWithConfig dynRecUnsafe defaultConfig

createSampleProposal
  :: (MonadNettest caps base m, HasCallStack)
  => Int -> Address -> TAddress Parameter -> m ProposalKey
createSampleProposal counter dodOwner dao = do
  let (pk, action) = createSampleProposal_ counter dodOwner dao
  withSender dodOwner action
  pure pk

createSampleProposal_
  :: (MonadOps m, HasCallStack)
  => Int -> Address -> TAddress Parameter -> (ProposalKey, m ())
createSampleProposal_ counter dodOwner1 dao =
  let params = ProposeParams
        { ppFrom = dodOwner1
        , ppFrozenToken = 10
        , ppProposalMetadata = lPackValueRaw @Integer $ fromIntegral counter
        }
  in (makeProposalKey params, call dao (Call @"Propose") params)

-- TODO consider making this polymorphic on the input/output size
createSampleProposals
  :: (MonadNettest caps base m, HasCallStack)
  => (Int, Int) -> Address -> TAddress Parameter -> m (ProposalKey, ProposalKey)
createSampleProposals (counter1, counter2) dodOwner1 dao = do
  let (pk1, action1) = createSampleProposal_ counter1 dodOwner1 dao
  let (pk2, action2) = createSampleProposal_ counter2 dodOwner1 dao
  withSender dodOwner1 . inBatch $ do
    action1
    action2
    return ()
  pure (pk1, pk2)

checkStorage
  :: forall caps base m st.
      ( AsRPC st ~ st, MonadNettest caps base m
      , HasCallStack, Eq st, NiceStorage st, NiceUnpackedValue st)
  => Address -> st -> m ()
checkStorage addr expected = do
  realSt <- getStorage @st addr
  assert (expected == realSt) "Unexpected storage"
