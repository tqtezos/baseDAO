-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Types mirrored from LIGO implementation.
module Ligo.BaseDAO.Types
  ( frozenTokenId
  , baseDaoAnnOptions

    -- * Operators
  , Operator
  , Operators

    -- * Ledger
  , Ledger
  , LedgerKey
  , LedgerValue

    -- * Total Supply
  , TotalSupply

    -- * Proposals
  , ProposalKey
  , ProposeParams(..)
  , GovernanceToken(..)

    -- * Voting
  , QuorumThreshold (..)
  , VotingPeriod
  , VoteParam (..)
  , Voter (..)

    -- * Non FA2
  , BurnParam (..)
  , MintParam (..)
  , TransferContractTokensParam (..)

    -- * Permissions
  , Nonce (..)
  , DataToSign (..)
  , Permit (..)
  , PermitProtected (..)
  , pattern NoPermit

    -- * Storage/Parameter
  , TransferOwnershipParam
  , ProposalMetadata
  , ContractExtra
  , CustomEntrypoints
  , Proposal (..)
  , Parameter
  , Storage (..)
  , Config (..)
  , FullStorage (..)
  , AddressFreezeHistory (..)
  , LastPeriodChange (..)
  , DynamicRec (..)
  , dynRecUnsafe
  , mkStorage
  , mkMetadataMap
  , mkConfig
  , defaultConfig
  , mkFullStorage

  , sOperatorsLens
  ) where

import Universum (One(..), maybe, (*))

import Control.Lens (makeLensesFor)
import qualified Data.Map as M

import Lorentz hiding (now)
import Lorentz.Annotation ()
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import qualified Lorentz.Contracts.Spec.TZIP16Interface as TZIP16
import Michelson.Typed.Annotation
import Michelson.Typed.T (T(TUnit))
import Michelson.Untyped.Annotation
import Util.Markdown

------------------------------------------------------------------------
-- Orphans
------------------------------------------------------------------------

customGeneric "FA2.Parameter" ligoLayout

deriving anyclass instance IsoValue FA2.Parameter

instance ParameterHasEntrypoints FA2.Parameter where
  type ParameterEntrypointsDerivation FA2.Parameter = EpdPlain

instance TypeHasDoc FA2.Parameter where
  typeDocMdDescription = "Describes the FA2 operations."

instance HasAnnotation FA2.Parameter

------------------------------------------------------------------------
-- Misc
------------------------------------------------------------------------

frozenTokenId :: FA2.TokenId
frozenTokenId = FA2.TokenId 0

baseDaoAnnOptions :: AnnOptions
baseDaoAnnOptions = defaultAnnOptions { fieldAnnModifier = dropPrefixThen toSnake }

------------------------------------------------------------------------
-- Operators
------------------------------------------------------------------------

type Operator = ("owner" :! Address, "operator" :! Address)

type Operators = BigMap Operator ()

------------------------------------------------------------------------
-- Ledger
------------------------------------------------------------------------

type Ledger = BigMap LedgerKey LedgerValue

type LedgerKey = (Address, FA2.TokenId)

type LedgerValue = Natural

------------------------------------------------------------------------
-- Total Supply
------------------------------------------------------------------------

type TotalSupply = Map FA2.TokenId Natural

------------------------------------------------------------------------
-- Proposals
------------------------------------------------------------------------

data ProposeParams = ProposeParams
  { ppFrozenToken      :: Natural
  --  ^ Determines how many sender's tokens will be frozen to get
  -- the proposal accepted
  , ppProposalMetadata :: ProposalMetadata
  }
  deriving stock (Generic, Show)
  deriving anyclass IsoValue

instance TypeHasDoc ProposalMetadata => TypeHasDoc ProposeParams where
  typeDocMdDescription =
     "Describes the how many proposer's frozen tokens will be frozen and the proposal metadata"
  typeDocMdReference = homomorphicTypeDocMdReference
  typeDocHaskellRep = concreteTypeDocHaskellRep @ProposeParams
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @ProposeParams

instance HasAnnotation ProposeParams where
  annOptions = baseDaoAnnOptions

type ProposalKey = Hash Blake2b $ Packed (ProposeParams, Address)

------------------------------------------------------------------------
-- Voting
------------------------------------------------------------------------

-- | QuorumThreshold that a proposal need to meet, expressed as a fraction of
-- the frozen tokens total supply.
-- A proposal will be rejected if the quorum_threshold is not met,
-- regardless of upvotes > downvotes
-- A proposal will be accepted only if:
-- (quorum_threshold * total_supply >= upvote + downvote) && (upvote > downvote)
data QuorumThreshold = QuorumThreshold
  { qtNumerator   :: Natural
  , qtDenominator :: Natural
  }
  deriving stock (Generic, Show)
  deriving anyclass IsoValue

instance HasAnnotation QuorumThreshold where
  annOptions = baseDaoAnnOptions

-- | Voting period in seconds
type VotingPeriod = Natural

-- | Represents whether a voter has voted against (False) or for (True) a given proposal.
type VoteType = Bool

data Voter = Voter
  { voterAddress :: Address
  , voteAmount :: Natural
  , voteType :: VoteType
  }
  deriving stock (Show)

instance TypeHasDoc Voter where
  typeDocMdDescription =
    "Describes a voter on some proposal, including its address, vote type and vote amount"

instance HasAnnotation Voter where
  annOptions = baseDaoAnnOptions

data VoteParam = VoteParam
  { vProposalKey :: ProposalKey
  , vVoteType    :: VoteType
  , vVoteAmount  :: Natural
  }
  deriving stock (Generic, Show)
  deriving anyclass IsoValue

instance TypeHasDoc ProposalMetadata => TypeHasDoc VoteParam where
  typeDocMdDescription = "Describes target proposal id, vote type and vote amount"
  typeDocMdReference = homomorphicTypeDocMdReference
  typeDocHaskellRep = concreteTypeDocHaskellRep @VoteParam
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @VoteParam

instance HasAnnotation VoteParam where
  annOptions = baseDaoAnnOptions

------------------------------------------------------------------------
-- Non FA2
------------------------------------------------------------------------

data BurnParam = BurnParam
  { bFrom_   :: Address
  , bTokenId :: FA2.TokenId
  , bAmount  :: Natural
  }
  deriving stock (Generic, Show)
  deriving anyclass IsoValue

instance TypeHasDoc BurnParam where
  typeDocMdDescription = "Describes whose account, which token id and in what amount to burn"

instance HasAnnotation BurnParam where
  annOptions = baseDaoAnnOptions

data MintParam = MintParam
  { mTo_     :: Address
  , mTokenId :: FA2.TokenId
  , mAmount  :: Natural
  }
  deriving stock (Generic, Show)
  deriving anyclass IsoValue

instance TypeHasDoc MintParam where
  typeDocMdDescription = "Describes whose account, which token id and in what amount to mint"

instance HasAnnotation MintParam where
  annOptions = baseDaoAnnOptions

data TransferContractTokensParam = TransferContractTokensParam
  { tcContractAddress :: Address
  , tcParams          :: FA2.TransferParams
  }
  deriving stock (Generic, Show)
  deriving anyclass IsoValue

instance TypeHasDoc TransferContractTokensParam where
  typeDocMdDescription =
    "Describes an FA2 contract address and the parameter to call its 'transfer' entrypoint"

instance HasAnnotation TransferContractTokensParam where
  annOptions = baseDaoAnnOptions

------------------------------------------------------------------------
-- Permissions
------------------------------------------------------------------------

newtype Nonce = Nonce { unNonce :: Natural }
  deriving stock (Generic, Show)
  deriving newtype (IsoValue, HasAnnotation)

instance TypeHasDoc Nonce where
  typeDocMdDescription =
    "Contract-local nonce used to make some data unique."

-- | When signing something, you usually want to use this wrapper over your data.
-- It ensures that replay attack is not possible.
data DataToSign d = DataToSign
  { dsChainId :: ChainId
  , dsContract :: Address
  , dsNonce :: Nonce
  , dsData :: d
  } deriving stock (Generic)
    deriving anyclass (IsoValue)

instance HasAnnotation d => HasAnnotation (DataToSign d) where
  annOptions = baseDaoAnnOptions

instance TypeHasDoc d => TypeHasDoc (DataToSign d) where
  typeDocMdDescription = [md|
    A wrapper over data that is to be signed.

    Aside from the original data, this contains elements that ensure the result
    to be globally unique in order to avoid replay attacks:
    * Chain id
    * Address of the contract
    * Nonce - suitable nonce can be fetched using the dedicated endpoint.
    |]
  typeDocMdReference = poly1TypeDocMdReference
  typeDocHaskellRep = concreteTypeDocHaskellRep @(DataToSign MText)
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @(DataToSign MText)

instance CanCastTo a b => DataToSign a `CanCastTo` DataToSign b where
  castDummy = castDummyG

-- | Information about permit.
--
-- Following TZIP-17, this allows a service to execute an entrypoint from
-- user's behalf securely.
--
-- Type argument @a@ stands for argument of entrypoint protected by permit.
data Permit a = Permit
  { pKey :: PublicKey
    -- ^ Key of the user.
  , pSignature :: TSignature $ Packed (DataToSign a)
    -- ^ Parameter signature.
  } deriving stock (Generic, Show)
    deriving anyclass (IsoValue)

instance HasAnnotation (Permit a) where
  annOptions = baseDaoAnnOptions

instance TypeHasDoc a => TypeHasDoc (Permit a) where
  typeDocMdDescription = [md|
    Permission for executing an action from another user's behalf.

    This contains public key of that user and signed argument for the entrypoint.
    Type parameter of `Permit` stands for the entrypoint argument type.
    |]
  typeDocMdReference = poly1TypeDocMdReference
  typeDocHaskellRep = concreteTypeDocHaskellRep @(Permit Integer)
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @(Permit Integer)

-- | Parameter, optionally protected with permission.
--
-- If permit is not provided, we treat the current sender as
-- original author of this call.
-- Otherwise we ensure that parameter is indeed signed by the
-- author of the key in permit.
data PermitProtected a = PermitProtected
  { ppArgument :: a
  , ppPermit :: Maybe (Permit a)
  } deriving stock (Generic, Show)
    deriving anyclass (IsoValue)

instance HasAnnotation a => HasAnnotation (PermitProtected a) where
  getAnnotation _ =
    NTPair
      [typeAnnQ|permit_protected|]
      (noAnn @FieldTag)
      [fieldAnnQ|permit|]
      noAnn noAnn
      (getAnnotation @a NotFollowEntrypoint)
      (getAnnotation @(Maybe (Permit a)) NotFollowEntrypoint)
    -- TODO: propably it is not assumed to look this way,
    --       rewrite in a prettier way somehow?

-- | Perform operation from sender behalf.
pattern NoPermit :: a -> PermitProtected a
pattern NoPermit a = PermitProtected a Nothing

-- | Type which we know nothing about.
data SomeType

instance TypeHasDoc SomeType where
  typeDocName _ = "SomeType"
  typeDocMdDescription = "Some type, may differ in various situations."
  typeDocDependencies _ = []
  typeDocHaskellRep _ _ = Nothing
  typeDocMichelsonRep _ = (Just "SomeType", TUnit)

instance a `CanCastTo` SomeType

------------------------------------------------------------------------
-- Storage/Parameter
------------------------------------------------------------------------

type TransferOwnershipParam = ("newOwner" :! Address)

-- | Represents a product type with arbitrary fields.
--
-- Contains a name to make different such records distinguishable.
newtype DynamicRec n = DynamicRec { unDynamic :: Map MText ByteString }
  deriving stock (Generic, Show, Eq)
  deriving newtype (IsoValue, HasAnnotation, Default, One, Semigroup)

-- | Construct 'DynamicRec' assuming it contains no mandatory entries.
dynRecUnsafe :: DynamicRec n
dynRecUnsafe = DynamicRec mempty

-- TODO consider making these all 'BigMap's instead
type ProposalMetadata = DynamicRec "pm"
type ContractExtra = DynamicRec "ce"
type CustomEntrypoints = DynamicRec "ep"


data Proposal = Proposal
  { plUpvotes                 :: Natural
  , plDownvotes               :: Natural
  , plStartDate               :: Timestamp
  , plPeriodNum               :: Natural

  , plMetadata                :: ProposalMetadata

  , plProposer                :: Address
  , plProposerFrozenToken     :: Natural
  , plProposerFixedFeeInToken :: Natural

  , plVoters                  :: [Voter]
  }
  deriving stock (Show)

-- | Utility type containing an entrypoint name and its packed argument.
type CallCustomParam = (MText, ByteString)

-- | Utility type containing an entrypoint name and its packed lambda.
type CustomEntrypoint = (MText, ByteString)

type FreezeParam = ("amount" :! Natural)
type UnfreezeParam = ("amount" :! Natural)

-- NOTE: Constructors of the parameter types should remain sorted for the
--'ligoLayout' custom derivation to work.
--
-- TODO: This above will be no longer the case once the following issue is fixed.
-- https://gitlab.com/morley-framework/morley/-/issues/527
data ForbidXTZParam
  = Accept_ownership ()
  | Burn BurnParam
  | Call_FA2 FA2.Parameter
  | Drop_proposal ProposalKey
  | Flush Natural
  | Freeze FreezeParam
  | Get_vote_permit_counter (Void_ () Nonce)
  | Get_total_supply (Void_ FA2.TokenId Natural)
  | Mint MintParam
  | Set_fixed_fee_in_token Natural
  | Set_quorum_threshold QuorumThreshold
  | Set_voting_period VotingPeriod
  | Transfer_ownership TransferOwnershipParam
  | Unfreeze UnfreezeParam
  | Vote [PermitProtected VoteParam]
  deriving stock (Show)

data AllowXTZParam
  = CallCustom CallCustomParam
  | Propose ProposeParams
  | Transfer_contract_tokens TransferContractTokensParam
  deriving stock (Show)

data Parameter
  = XtzAllowed AllowXTZParam
  | XtzForbidden ForbidXTZParam
  deriving stock (Show)

data LastPeriodChange = LastPeriodChange
  { vhPeriodNum :: Natural
  , vhChangedOn :: Timestamp
  } deriving stock (Eq, Show)

data AddressFreezeHistory = AddressFreezeHistory
  { fhCurrentUnstaked :: Natural
  , fhCurrentPeriodNum :: Natural
  , fhStaked :: Natural
  } deriving stock (Eq, Show)

data GovernanceToken = GovernanceToken
  { gtAddress :: Address
  , gtTokenId :: FA2.TokenId
  } deriving stock (Eq, Show)

customGeneric "GovernanceToken" ligoLayout
deriving anyclass instance IsoValue GovernanceToken


type Cycle = Natural
type Period = Natural
type Level = Natural

data VotingPeriodParams = VotingPeriodParams
  { vpLevelsPerCycle :: Natural
  , vpCyclesPerPeriod :: Natural
  , vpLevelsPerCycleChangeAt :: (Cycle, Level)
  , vpCyclesPerPeriodChangeAt :: (Period, Cycle)
  } deriving stock (Eq, Show)

customGeneric "VotingPeriodParams" ligoLayout
deriving anyclass instance IsoValue VotingPeriodParams

data Storage = Storage
  { sAdmin :: Address
  , sExtra :: ContractExtra
  , sFrozenTokenId :: FA2.TokenId
  , sLedger :: Ledger
  , sMetadata :: TZIP16.MetadataMap BigMap
  , sOperators :: Operators
  , sPendingOwner :: Address
  , sPermitsCounter :: Nonce
  , sProposals :: BigMap ProposalKey Proposal
  , sProposalKeyListSortByDate :: Set (Timestamp, ProposalKey)
  , sQuorumThreshold :: QuorumThreshold
  , sGovernanceToken :: GovernanceToken
  , sVotingPeriod :: VotingPeriod
  , sTotalSupply :: TotalSupply
  , sFreezeHistory :: BigMap Address AddressFreezeHistory
  , sFixedProposalFeeInToken :: Natural
  , sVotingPeriodParams :: VotingPeriodParams
  }
  deriving stock (Show)

instance HasAnnotation VotingPeriodParams where
  annOptions = baseDaoAnnOptions

instance HasAnnotation GovernanceToken where
  annOptions = baseDaoAnnOptions

instance HasAnnotation Proposal where
  annOptions = baseDaoAnnOptions

instance HasAnnotation Storage where
  annOptions = baseDaoAnnOptions

instance HasAnnotation AddressFreezeHistory where
  annOptions = baseDaoAnnOptions

instance HasAnnotation FullStorage where
  annOptions = baseDaoAnnOptions

instance HasAnnotation Config where
  annOptions = baseDaoAnnOptions

instance HasAnnotation LastPeriodChange where
  annOptions = baseDaoAnnOptions

instance HasFieldOfType Storage name field => StoreHasField Storage name field where
  storeFieldOps = storeFieldOpsADT

mkStorage
  :: "admin" :! Address
  -> "votingPeriod" :? Natural
  -> "quorumThreshold" :? QuorumThreshold
  -> "extra" :! ContractExtra
  -> "metadata" :! TZIP16.MetadataMap BigMap
  -> "now" :! Timestamp
  -> "tokenAddress" :! Address
  -> Storage
mkStorage admin votingPeriod quorumThreshold extra metadata now tokenAddress =
  Storage
    { sAdmin = arg #admin admin
    , sExtra = arg #extra extra
    , sLedger = mempty
    , sMetadata = arg #metadata metadata
    , sOperators = mempty
    , sPendingOwner = arg #admin admin
    , sPermitsCounter = Nonce 0
    , sProposals = mempty
    , sProposalKeyListSortByDate = mempty
    , sQuorumThreshold = argDef #quorumThreshold quorumThresholdDef quorumThreshold
    , sGovernanceToken = GovernanceToken
        { gtAddress = arg #tokenAddress tokenAddress
        , gtTokenId = FA2.theTokenId
        }
    , sVotingPeriod = argDef #votingPeriod votingPeriodDef votingPeriod
    , sTotalSupply = M.fromList [(frozenTokenId, 0)]
    , sFreezeHistory = mempty
    , sFixedProposalFeeInToken = 0
    , sFrozenTokenId = frozenTokenId
    , sVotingPeriodParams = VotingPeriodParams
        { vpLevelsPerCycle = 4096
        , vpCyclesPerPeriod = 5
        , vpCyclesPerPeriodChangeAt = (0, 0)
        , vpLevelsPerCycleChangeAt = (0, 0)
        }
    }
  where
    votingPeriodDef = 60 * 60 * 24 * 7  -- 7 days
    quorumThresholdDef = QuorumThreshold 1 10 -- 10% of frozen total supply

mkMetadataMap
  :: "metadataHostAddress" :! Address
  -> "metadataHostChain" :? TZIP16.ExtChainId
  -> "metadataKey" :! MText
  -> TZIP16.MetadataMap BigMap
mkMetadataMap hostAddress hostChain key =
  TZIP16.metadataURI . TZIP16.tezosStorageUri host $ arg #metadataKey key
  where
    host = maybe
      TZIP16.contractHost
      TZIP16.foreignContractHost
      (argF #metadataHostChain hostChain)
      (arg #metadataHostAddress hostAddress)

data Config = Config
  { cProposalCheck :: '[ProposeParams, ContractExtra] :-> '[Bool]
  , cRejectedProposalReturnValue :: '[Proposal, ContractExtra] :-> '["slash_amount" :! Natural]
  , cDecisionLambda :: '[Proposal, ContractExtra] :-> '[List Operation, ContractExtra]

  , cMaxProposals :: Natural
  , cMaxVotes :: Natural
  , cMaxQuorumThreshold :: QuorumThreshold
  , cMinQuorumThreshold :: QuorumThreshold

  , cMaxVotingPeriod :: Natural
  , cMinVotingPeriod :: Natural

  , cCustomEntrypoints :: CustomEntrypoints
  }
  deriving stock (Show)

mkConfig :: [CustomEntrypoint] -> Config
mkConfig customEps = Config
  { cProposalCheck = do
      dropN @2; push True
  , cRejectedProposalReturnValue = do
      dropN @2; push (0 :: Natural); toNamed #slash_amount
  , cDecisionLambda = do
      drop; nil
  , cCustomEntrypoints = DynamicRec $ M.fromList customEps

  , cMaxVotingPeriod = 60 * 60 * 24 * 30
  , cMinVotingPeriod = 1

  , cMaxQuorumThreshold = QuorumThreshold 99 100 -- 99%
  , cMinQuorumThreshold = QuorumThreshold 1 100 -- 1%

  , cMaxVotes = 1000
  , cMaxProposals = 500
  }

defaultConfig :: Config
defaultConfig = mkConfig []

data FullStorage = FullStorage
  { fsStorage :: Storage
  , fsConfig :: Config
  }
  deriving stock (Show)

mkFullStorage
  :: "admin" :! Address
  -> "votingPeriod" :? Natural
  -> "quorumThreshold" :? QuorumThreshold
  -> "extra" :! ContractExtra
  -> "metadata" :! TZIP16.MetadataMap BigMap
  -> "now" :! Timestamp
  -> "tokenAddress" :! Address
  -> "customEps" :? [CustomEntrypoint]
  -> FullStorage
mkFullStorage admin vp qt extra mdt now tokenAddress cEps = FullStorage
  { fsStorage = mkStorage admin vp qt extra mdt now tokenAddress
  , fsConfig  = mkConfig (argDef #customEps [] cEps)
  }

-- Instances
------------------------------------------------

deriving anyclass instance IsoValue Voter

customGeneric "Voter" ligoLayout
customGeneric "Proposal" ligoLayout
deriving anyclass instance IsoValue Proposal

customGeneric "ForbidXTZParam" ligoLayout
deriving anyclass instance IsoValue ForbidXTZParam
instance ParameterHasEntrypoints ForbidXTZParam where
  type ParameterEntrypointsDerivation ForbidXTZParam = EpdDelegate

customGeneric "AllowXTZParam" ligoLayout
deriving anyclass instance IsoValue AllowXTZParam
instance ParameterHasEntrypoints AllowXTZParam where
  type ParameterEntrypointsDerivation AllowXTZParam = EpdDelegate

customGeneric "Parameter" ligoLayout
deriving anyclass instance IsoValue Parameter
instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdDelegate

customGeneric "AddressFreezeHistory" ligoLayout
deriving anyclass instance IsoValue AddressFreezeHistory

customGeneric "LastPeriodChange" ligoLayout
deriving anyclass instance IsoValue LastPeriodChange

customGeneric "Storage" ligoLayout
deriving anyclass instance IsoValue Storage

customGeneric "Config" ligoLayout
deriving anyclass instance IsoValue Config

deriving stock instance Generic FullStorage
deriving anyclass instance IsoValue FullStorage

-- Lenses
------------------------------------------------

makeLensesFor
  [ ("sOperators", "sOperatorsLens")
  ] ''Storage

------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------

type instance ErrorArg "nOT_OWNER" = NoErrorArg

instance CustomErrorHasDoc "nOT_OWNER" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "The sender of transaction is not owner"

type instance ErrorArg "fROZEN_TOKEN_NOT_TRANSFERABLE" = NoErrorArg

instance CustomErrorHasDoc "fROZEN_TOKEN_NOT_TRANSFERABLE" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "The sender tries to transfer frozen token"

type instance ErrorArg "nOT_PENDING_ADMIN" = NoErrorArg

instance CustomErrorHasDoc "nOT_PENDING_ADMIN" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Received an `accept_ownership` from an address other than what is in the pending owner field"

type instance ErrorArg "nOT_ENOUGH_FROZEN_TOKENS" = ()

instance CustomErrorHasDoc "nOT_ENOUGH_FROZEN_TOKENS" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "There were not enough frozen tokens for the operation"

type instance ErrorArg "nOT_PROPOSING_PERIOD" = NoErrorArg

instance CustomErrorHasDoc "nOT_PROPOSING_PERIOD" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Proposal creation attempted in non-proposal period"

type instance ErrorArg "nOT_ADMIN" = NoErrorArg

instance CustomErrorHasDoc "nOT_ADMIN" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Received an operation that require administrative privileges\
    \ from an address that is not the current administrator"

type instance ErrorArg "fORBIDDEN_XTZ" = NoErrorArg

instance CustomErrorHasDoc "fORBIDDEN_XTZ" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Received some XTZ as part of a contract call, which is forbidden"

type instance ErrorArg "fAIL_PROPOSAL_CHECK" = NoErrorArg

instance CustomErrorHasDoc "fAIL_PROPOSAL_CHECK" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to propose a proposal that does not pass `proposalCheck`"

type instance ErrorArg "pROPOSAL_INSUFFICIENT_BALANCE" = NoErrorArg

instance CustomErrorHasDoc "pROPOSAL_INSUFFICIENT_BALANCE" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to propose a proposal without having enough unfrozen token"

type instance ErrorArg "vOTING_INSUFFICIENT_BALANCE" = NoErrorArg

instance CustomErrorHasDoc "vOTING_INSUFFICIENT_BALANCE" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to vote on a proposal without having enough unfrozen token"

type instance ErrorArg "pROPOSAL_NOT_EXIST" = NoErrorArg

instance CustomErrorHasDoc "pROPOSAL_NOT_EXIST" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to vote on a proposal that does not exist"

type instance ErrorArg "vOTING_PERIOD_OVER" = NoErrorArg

instance CustomErrorHasDoc "vOTING_PERIOD_OVER" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to vote on a proposal that is already ended"

------------------------------------------------
-- Error causes by bounded value
------------------------------------------------

type instance ErrorArg "oUT_OF_BOUND_VOTING_PERIOD" = NoErrorArg

instance CustomErrorHasDoc "oUT_OF_BOUND_VOTING_PERIOD" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to set voting period that is out of bound."

type instance ErrorArg "oUT_OF_BOUND_QUORUM_THRESHOLD" = NoErrorArg

instance CustomErrorHasDoc "oUT_OF_BOUND_QUORUM_THRESHOLD" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to set quorum threshold that is out of bound"

type instance ErrorArg "mAX_PROPOSALS_REACHED" = NoErrorArg

instance CustomErrorHasDoc "mAX_PROPOSALS_REACHED" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to propose a proposal when proposals max amount is already reached"

type instance ErrorArg "mAX_VOTES_REACHED" = NoErrorArg

instance CustomErrorHasDoc "mAX_VOTES_REACHED" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to vote on a proposal when the votes max amount of that proposal is already reached"

type instance ErrorArg "pROPOSER_NOT_EXIST_IN_LEDGER" = NoErrorArg

instance CustomErrorHasDoc "pROPOSER_NOT_EXIST_IN_LEDGER" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Expect a proposer address to exist in Ledger but it is not found (Impossible Case)"

type instance ErrorArg "pROPOSAL_NOT_UNIQUE" = NoErrorArg

instance CustomErrorHasDoc "pROPOSAL_NOT_UNIQUE" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to propose a proposal that is already existed in the Storage."

type instance ErrorArg "fAIL_TRANSFER_CONTRACT_TOKENS" = NoErrorArg

instance CustomErrorHasDoc "fAIL_TRANSFER_CONTRACT_TOKENS" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Trying to cross-transfer BaseDAO tokens to another contract that does not exist or is not a valid FA2 contract."

type instance ErrorArg "mISSIGNED" = Packed (DataToSign SomeType)

instance CustomErrorHasDoc "mISSIGNED" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Invalid signature provided."

type instance ErrorArg "bAD_ENTRYPOINT_PARAMETER" = NoErrorArg

instance CustomErrorHasDoc "bAD_ENTRYPOINT_PARAMETER" where
  customErrClass = ErrClassBadArgument
  customErrDocMdCause = "Value passed to the entrypoint is not valid"

type instance ErrorArg "fAIL_DROP_PROPOSAL_NOT_OVER" = NoErrorArg

instance CustomErrorHasDoc "fAIL_DROP_PROPOSAL_NOT_OVER" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "An error occurred when trying to drop a proposal due to the proposal's voting period is not over"

type instance ErrorArg "fAIL_DROP_PROPOSAL_NOT_ACCEPTED" = NoErrorArg

instance CustomErrorHasDoc "fAIL_DROP_PROPOSAL_NOT_ACCEPTED" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "An error occurred when trying to drop a proposal due to the proposal is not an accepted proposal"

type instance ErrorArg "nEGATIVE_TOTAL_SUPPLY" = ()

instance CustomErrorHasDoc "nEGATIVE_TOTAL_SUPPLY" where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "An error occured when trying to burn an amount of token more than its current total supply"
