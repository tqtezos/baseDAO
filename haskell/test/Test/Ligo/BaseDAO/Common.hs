-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
{-# LANGUAGE NumericUnderscores #-}

-- TODO: Replace 'Empty' with 'Never' from morley
{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.Ligo.BaseDAO.Common
  ( OriginateFn
  , totalSupplyFromLedger

  , mkFA2View
  , addressToKeyHash
  , checkTokenBalance
  , dummyFA2Contract
  , makeProposalKey
  , addDataToSign
  , permitProtect
  , sendXtz
  , ProposalMetadataFromNum (..)

  , createSampleProposal
  , originateLigoDaoWithBalance
  , originateLigoDaoWithConfigDesc
  , originateLigoDao
  ) where

import Universum

import Lorentz hiding (now, (>>))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Test (contractConsumer)
import Michelson.Typed.Convert (convertContract, untypeValue)
import Morley.Nettest
import Named ((!))
import Time (sec)
import Util.Named
import Tezos.Address (Address(..))

import qualified Data.Map as M
import qualified Data.Set as S
import Ligo.BaseDAO.Contract
import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Proposal.Config (ConfigDesc, fillConfig)

type OriginateFn m = m ((Address, Address), (Address, Address), TAddress Parameter, TAddress FA2.Parameter, Address)

addressToKeyHash :: Address -> KeyHash
addressToKeyHash (KeyAddress h) = h
addressToKeyHash _ = error "Not an implicit address"

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

totalSupplyFromLedger :: Ledger -> TotalSupply
totalSupplyFromLedger (BigMap ledger) =
  M.foldrWithKey (\(_, tokenId) val totalSup ->
      M.adjust ((+) val) tokenId totalSup
    )
    (M.fromList [(frozenTokenId, 0)])
    ledger

-- | Create FA2 View
mkFA2View
  :: forall paramFa a r contract. ToContractRef r contract
  => a
  -> contract
  -> FA2.FA2View paramFa a r
mkFA2View a c = FA2.FA2View (mkView a c)

-- | Helper function to check a user balance of a particular token
checkTokenBalance
  :: (MonadNettest caps base m, HasCallStack)
  => FA2.TokenId -> TAddress Parameter
  -> Address -> Natural
  -> m ()
checkTokenBalance tokenId dao addr expectedValue = withFrozenCallStack $ do
  consumer <- originateSimple "consumer" [] contractConsumer

  withSender (AddressResolved addr) $ call dao (Call @"Balance_of")
    (mkFA2View [ FA2.BalanceRequestItem
      { briOwner = addr
      , briTokenId = tokenId
      } ] consumer)

  checkStorage (AddressResolved $ toAddress consumer)
    (toVal [[((addr, tokenId), expectedValue)]] )

makeProposalKey :: ProposeParams -> Address -> ProposalKey
makeProposalKey params owner = toHashHs $ lPackValue (params, owner)

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
  => AddressOrAlias -> (DataToSign a, a) -> m (PermitProtected a)
permitProtect author (toSign, a) = do
  authorAlias <- getAlias author
  pKey <- getPublicKey author
  pSignature <- signBinary (lPackValue toSign) authorAlias
  return PermitProtected
    { ppArgument = a
    , ppPermit = Just Permit{..}
    }

sendXtz
  :: (MonadNettest caps base m, HasCallStack, NiceParameter pm)
  => Address -> EpName -> pm -> m ()
sendXtz addr epName pm = withFrozenCallStack $ do
  let transferData = TransferData
        { tdTo = AddressResolved addr
        , tdAmount = toMutez 0.5_e6 -- 0.5 xtz
        , tdEntrypoint = epName
        , tdParameter = pm
        }
  transfer transferData

-- | Since in LIGO proposal metadata type is fixed but is inconvenient to work
-- with, we need a way to abstract away from that complexity - this problem
-- is resolved by the following typeclass.
class (KnownValue n, NicePackedValue n) => ProposalMetadataFromNum n where
  -- | Generate a proposal metadata.
  -- Different numbers must result in different values.
  proposalMetadataFromNum :: Int -> n

instance ProposalMetadataFromNum Integer where
  proposalMetadataFromNum = fromIntegral

instance ProposalMetadataFromNum ProposalMetadata where
  proposalMetadataFromNum n =
    one ([mt|int|], lPackValueRaw @Integer $ fromIntegral n)

-- TODO: Implement this via [#31] instead
-- checkIfAProposalExist
--   :: MonadNettest caps base m
--   => ProposalKey -> TAddress (Parameter TestProposalMetadata) -> m ()
-- checkIfAProposalExist proposalKey dao = do
--   owner :: Address <- newAddress "owner"
--   consumer <- originateSimple "consumer" [] contractConsumer
--   -- | If the proposal exists, there should be no error
--   callFrom (AddressResolved owner) dao (Call @"Proposal_metadata") (mkView proposalKey consumer)

-- TODO [#31]: See this ISSUES: https://gitlab.com/morley-framework/morley/-/issues/415#note_435327096
-- Check if certain field in storage
-- checkPropertyOfProposal :: _
-- checkPropertyOfProposal = error "undefined"

originateLigoDaoWithBalance
 :: MonadNettest caps base m
 => ContractExtra
 -> Config
 -> (Address -> Address -> [(LedgerKey, LedgerValue)])
 -> OriginateFn m
originateLigoDaoWithBalance extra config balFunc = do
  owner1 :: Address <- newAddress "owner1"
  operator1 :: Address <- newAddress "operator1"
  owner2 :: Address <- newAddress "owner2"
  operator2 :: Address <- newAddress "operator2"

  admin :: Address <- newAddress "admin"

  let bal = BigMap $ M.fromList $ balFunc owner1 owner2
  let operators = BigMap $ M.fromSet (const ()) $ S.fromList
        [ (#owner .! owner1, #operator .! operator1)
        , (#owner .! owner2, #operator .! operator2)
        ]

  now <- getNow
  tokenContract <- originateSimple "TokenContract" [] dummyFA2Contract

  let fullStorage = FullStorage
        { fsStorage =
            ( mkStorage
              ! #extra extra
              ! #admin admin
              ! #votingPeriod (cMinVotingPeriod config)
              ! #quorumThreshold (cMinQuorumThreshold config)
              ! #metadata mempty
              ! #tokenAddress (unTAddress tokenContract)
              ! #now now
            )
            { sLedger = bal
            , sOperators = operators
            , sTotalSupply = totalSupplyFromLedger bal
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
  daoUntyped <- originateLargeUntyped originateData
  let dao = TAddress @Parameter daoUntyped

  pure ((owner1, operator1), (owner2, operator2), dao, tokenContract, admin)

originateLigoDaoWithConfig
 :: MonadNettest caps base m
 => ContractExtra
 -> Config
 -> OriginateFn m
originateLigoDaoWithConfig extra config =
  originateLigoDaoWithBalance extra config
    (\owner1_ owner2_ ->
    [ ((owner1_, frozenTokenId), 100)
    , ((owner2_, frozenTokenId), 100)
    ])

originateLigoDaoWithConfigDesc
 :: MonadNettest caps base m
 => ContractExtra
 -> ConfigDesc Config
 -> OriginateFn m
originateLigoDaoWithConfigDesc extra config =
  originateLigoDaoWithBalance extra (fillConfig config defaultConfig)
    (\owner1_ owner2_ ->
    [ ((owner1_, frozenTokenId), 100)
    , ((owner2_, frozenTokenId), 100)
    ])

originateLigoDao :: MonadNettest caps base m => OriginateFn m
originateLigoDao =
  originateLigoDaoWithConfig dynRecUnsafe defaultConfig

createSampleProposal
  :: (MonadNettest caps base m, HasCallStack)
  => Int -> Int -> Address -> TAddress Parameter -> m ProposalKey
createSampleProposal counter vp owner1 dao = do
  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = proposalMetadataFromNum counter
        }

  when (vp > 0) $ do
    withSender (AddressResolved owner1) $
      call dao (Call @"Freeze") (#amount .! 10, #keyhash .! (addressToKeyHash owner1))
    advanceTime (sec (fromIntegral vp))

  withSender (AddressResolved owner1) $ call dao (Call @"Propose") params
  pure $ (makeProposalKey params owner1)
