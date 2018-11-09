module Dscp.Snowdrop.Configuration
    ( DscpSigScheme
    , toDscpPK
    , toDscpSig

    , SHeader
    , SPayload (..)
    , SBlock
    , SUndo
    , SBlund
    , SStateTx
    , sBlockReconstruct

    , tipPrefix
    , blockPrefix
    , accountPrefix
    , publicationIdsPrefix
    , publicationOfPrefix
    , publicationHeadPrefix
    , txPrefix
    , txBlockPrefix
    , txOfPrefix
    , txHeadPrefix
    , nextBlockPrefix
    , blockIdxPrefix
    , privateBlockTxPrefix
    , blockPrefixes
    , Ids (..)
    , Values (..)

    , CanVerifyPayload
    , PersonalisedProof (..)
    , AddrTxProof
    , PublicationTxProof
    , Proofs (..)
    , Exceptions (..)
    , _AccountError
    , _PublicationError
    , _LogicError

    , TxIds (..)

    , BlockPlusAVLComposition
    ) where


import Control.Lens (makePrisms)
import Data.Reflection (Reifies (..))
import qualified Data.Set as S
import qualified Data.Text.Buildable as B
import Fmt (build, (+|))
import qualified Text.Show

import Snowdrop.Block (Block (..), BlockApplicationException, BlockRef (..), BlockStateException,
                       Blund (buBlock), CurrentBlockRef (..), TipKey, TipValue)
import Snowdrop.Core (CSMappendException, IdSumPrefixed (..), Prefix (..), RedundantIdException,
                      SValue, StateModificationException, StatePException, StateTx (..),
                      TxValidationException, Undo, ValidatorExecException)
import Snowdrop.Execution (RestrictionInOutException)
import Snowdrop.Util (HasReview (..), IdStorage, VerifySign, WithSignature (..), deriveIdView,
                      deriveView, verifySignature, withInj, withInjProj)
import qualified Snowdrop.Util as SD (PublicKey, Signature)

import Dscp.Core (Fees, HeaderHash)
import qualified Dscp.Core.Foundation as T
import Dscp.Crypto (HasAbstractSignature, Hash, PublicKey, SigScheme, Signature, hashF, verify)
import Dscp.Snowdrop.Storage.Types
import Dscp.Snowdrop.Types (Account, AccountException, AccountId (..), AccountTxTypeId (..),
                            PublicationException, PublicationTxTypeId (..))
import Dscp.Witness.Logic.Exceptions (LogicException)

----------------------------------------------------------------------------
-- Snowdrop signing types
----------------------------------------------------------------------------

data DscpSigScheme
data instance SD.PublicKey DscpSigScheme = DscpPK PublicKey
    deriving (Eq, Show, Generic)
data instance SD.Signature DscpSigScheme msg = DscpSig (Signature msg)
    deriving (Eq, Show, Generic)

toDscpPK :: PublicKey -> SD.PublicKey DscpSigScheme
toDscpPK = DscpPK

toDscpSig :: Signature msg -> SD.Signature DscpSigScheme msg
toDscpSig = DscpSig

instance Buildable (SD.PublicKey DscpSigScheme) where
    build (DscpPK key) = build key

instance Buildable (SD.Signature DscpSigScheme msg) where
    build (DscpSig sig) = build sig

instance HasAbstractSignature SigScheme msg => VerifySign DscpSigScheme msg where
    verifySignature (DscpPK pk) msg (DscpSig sig) = verify pk msg sig


----------------------------------------------------------------------------
-- Snowdrop block-related types
----------------------------------------------------------------------------

type SHeader  = T.Header
data SPayload = SPayload { sPayStateTxs     :: ![SStateTx]
                         , sPayOrigBodyHash :: !(Hash T.BlockBody)
                         } deriving (Eq, Show, Generic)
type SBlock   = Block SHeader SPayload
type SUndo    = Undo Ids Values
type SBlund   = Blund SHeader T.BlockBody SUndo

sBlockReconstruct :: SBlund -> T.Block
sBlockReconstruct (buBlock -> Block h b) = T.Block h b

type SStateTx = StateTx Ids Proofs Values

----------------------------------------------------------------------------
-- Identities/prefixes
----------------------------------------------------------------------------

tipPrefix :: Prefix
tipPrefix = Prefix 1

blockPrefix :: Prefix
blockPrefix = Prefix 2

accountPrefix :: Prefix
accountPrefix = Prefix 3

publicationOfPrefix :: Prefix
publicationOfPrefix = Prefix 4

publicationHeadPrefix :: Prefix
publicationHeadPrefix = Prefix 5

txPrefix :: Prefix
txPrefix = Prefix 6

txOfPrefix :: Prefix
txOfPrefix = Prefix 7

txHeadPrefix :: Prefix
txHeadPrefix = Prefix 8

nextBlockPrefix :: Prefix
nextBlockPrefix = Prefix 9

blockIdxPrefix :: Prefix
blockIdxPrefix = Prefix 10

publicationIdsPrefix :: Prefix
publicationIdsPrefix = Prefix 11

txBlockPrefix :: Prefix
txBlockPrefix = Prefix 12

privateBlockTxPrefix :: Prefix
privateBlockTxPrefix = Prefix 13

-- | Prefixes stored in block storage
blockPrefixes :: Set Prefix
blockPrefixes = S.fromList
    [ tipPrefix
    , blockPrefix
    , privateBlockTxPrefix
    , publicationIdsPrefix
    , publicationOfPrefix
    , publicationHeadPrefix
    , txPrefix
    , txBlockPrefix
    , txOfPrefix
    , txHeadPrefix
    , nextBlockPrefix
    , blockIdxPrefix
    ]

-- | Sum-type for all ids used within the application.
data Ids
    = TipKeyIds           TipKey
    | BlockRefIds        (BlockRef  HeaderHash)
    | AccountInIds        AccountId
    | TxIds               T.TxId
    | TxBlockIds          TxBlockRefId
    | TxOfIds             TxsOf
    | TxHeadIds           TxHead
    | PrivateBlockTx      T.PrivateHeaderHash
    | PublicationIds      T.PublicationTxId
    | PublicationOfIds    PublicationsOf
    | PublicationHeadIds  PublicationHead
    | NextBlockOfIds      NextBlockOf
    | BlockIdxIds         T.Difficulty
    deriving (Eq, Ord, Show, Generic)

instance Buildable Ids where
    build = ("Key " <>) . \case
        TipKeyIds           t            -> build t
        BlockRefIds        (BlockRef  r) -> "block ref " +| hashF r
        AccountInIds       (AccountId a) -> build a
        TxIds               ti           -> build ti
        TxBlockIds          ri           -> build ri
        TxOfIds             t            -> build t
        TxHeadIds           th           -> build th
        PrivateBlockTx      h            -> build h
        PublicationIds      i            -> build i
        PublicationOfIds    p            -> build p
        PublicationHeadIds  ph           -> build ph
        NextBlockOfIds      hh           -> build hh
        BlockIdxIds         d            -> build d

instance IdSumPrefixed Ids where
    idSumPrefix (TipKeyIds           _) = tipPrefix
    idSumPrefix (BlockRefIds         _) = blockPrefix
    idSumPrefix (AccountInIds        _) = accountPrefix
    idSumPrefix (TxIds               _) = txPrefix
    idSumPrefix (TxBlockIds          _) = txBlockPrefix
    idSumPrefix (TxOfIds             _) = txOfPrefix
    idSumPrefix (TxHeadIds           _) = txHeadPrefix
    idSumPrefix (PrivateBlockTx      _) = privateBlockTxPrefix
    idSumPrefix (PublicationIds      _) = publicationIdsPrefix
    idSumPrefix (PublicationOfIds    _) = publicationOfPrefix
    idSumPrefix (PublicationHeadIds  _) = publicationHeadPrefix
    idSumPrefix (NextBlockOfIds      _) = nextBlockPrefix
    idSumPrefix (BlockIdxIds         _) = blockIdxPrefix

instance HasReview Ids (BlockRef (CurrentBlockRef HeaderHash)) where
    inj (BlockRef (CurrentBlockRef h)) = BlockRefIds (BlockRef h)

----------------------------------------------------------------------------
-- Values
----------------------------------------------------------------------------

data Values
    = TipValueVal        (TipValue HeaderHash)
    | BlundVal            SBlund
    | AccountOutVal       Account
    | TxVal               TxItself
    | TxBlockVal          TxBlockRef
    | TxOfVal             LastTx
    | TxHeadVal           TxNext
    | PrivateBlockTxVal   T.PublicationTxId
    | PublicationVal      PublicationItself
    | PublicationOfVal    LastPublication
    | PublicationHeadVal  PublicationNext
    | NextBlockOfVal      NextBlock
    | BlockIdxVal         HeaderHash
    deriving (Eq, Show, Generic)

type instance SValue  TipKey               = TipValue HeaderHash
type instance SValue (BlockRef HeaderHash) = SBlund
type instance SValue  AccountId            = Account
type instance SValue  T.TxId               = TxItself
type instance SValue  TxBlockRefId         = TxBlockRef
type instance SValue  TxsOf                = LastTx
type instance SValue  TxHead               = TxNext
type instance SValue  T.PrivateHeaderHash  = T.PublicationTxId
type instance SValue  T.PublicationTxId    = PublicationItself
type instance SValue  PublicationsOf       = LastPublication
type instance SValue  PublicationHead      = PublicationNext
type instance SValue  NextBlockOf          = NextBlock
type instance SValue  T.Difficulty         = HeaderHash

----------------------------------------------------------------------------
-- Proofs
----------------------------------------------------------------------------

deriving instance (Eq (SD.Signature sigScheme a), Eq (SD.PublicKey sigScheme), Eq a) => Eq (WithSignature sigScheme a)
deriving instance (Show (SD.Signature sigScheme a), Show (SD.PublicKey sigScheme), Show a) => Show (WithSignature sigScheme a)

type CanVerifyPayload txid payload =
    VerifySign DscpSigScheme (txid, PublicKey, payload)

-- | Proof for transaction.
--   Allows to check that tx was not corrupted and is properly signed.
--   Also carries calculated fees.
data PersonalisedProof txid payload = PersonalisedProof
    { ppSignedPart :: WithSignature
        DscpSigScheme
        (txid, PublicKey, payload)
    , ppFees :: Fees
    }
    deriving (Eq, Show, Generic)

type AddrTxProof =
    PersonalisedProof T.TxId ()

type PublicationTxProof =
    PersonalisedProof T.PublicationTxId T.PrivateBlockHeader

data Proofs
    = AddressTxWitness     AddrTxProof         -- ^ Money transaction witness
    | PublicationTxWitness PublicationTxProof  -- ^ Publication transaction witness
    deriving (Eq, Show, Generic)

----------------------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------------------

data InternalExceptions
    = ExpanderRestrictionError   RestrictionInOutException
    | BlockStateError           (BlockStateException        Ids)
    | StateModificationError    (StateModificationException Ids)
    | RedundantIdError           RedundantIdException
    | ValidatorExecError         ValidatorExecException
    | CSMappendError            (CSMappendException         Ids)
    | TxValidationError          TxValidationException
    | StatePError                StatePException

instance Show InternalExceptions where
    show = toString . pretty

instance Buildable InternalExceptions where
    build = \case
        ExpanderRestrictionError err -> B.build err
        BlockStateError err -> B.build err
        StateModificationError err -> B.build err
        StatePError err -> B.build err
        RedundantIdError err -> B.build err
        ValidatorExecError err -> B.build err
        CSMappendError err -> B.build err
        TxValidationError err -> B.build err

data Exceptions
    = BlockApplicationError (BlockApplicationException  HeaderHash)
    | AccountError          AccountException
    | PublicationError      PublicationException
    | LogicError            LogicException
    | SdInternalError       InternalExceptions

makePrisms ''Exceptions

instance Exception Exceptions

instance Show Exceptions where
    show = toString . pretty

instance Buildable Exceptions where
    build = \case
        BlockApplicationError err -> B.build err
        AccountError err -> B.build err
        PublicationError err -> B.build err
        LogicError err -> B.build err
        SdInternalError err -> B.build err

----------------------------------------------------------------------------
-- TxIds
----------------------------------------------------------------------------

data TxIds
    = MoneyTxIds       AccountTxTypeId
    | PublicationTxIds PublicationTxTypeId
    deriving (Eq,Show)

instance Enum TxIds where
    toEnum = \case
        0 -> MoneyTxIds       AccountTxTypeId
        1 -> PublicationTxIds PublicationTxTypeId
        _ -> error "instance Enum TxIds"

    fromEnum (MoneyTxIds       AccountTxTypeId)     = 0
    fromEnum (PublicationTxIds PublicationTxTypeId) = 1

instance IdStorage TxIds AccountTxTypeId
instance IdStorage TxIds PublicationTxTypeId

----------------------------------------------------------------------------
-- Misc
----------------------------------------------------------------------------

data BlockPlusAVLComposition
instance Reifies BlockPlusAVLComposition (Set Prefix) where
    reflect _ = blockPrefixes

----------------------------------------------------------------------------
-- HasReview and lenses
----------------------------------------------------------------------------

makePrisms ''Values

deriveView withInjProj ''Ids
deriveIdView withInjProj ''Ids

deriveView withInjProj ''Values
deriveIdView withInjProj ''Values

deriveView withInjProj ''TxIds
deriveView withInjProj ''InternalExceptions
deriveView withInj ''Exceptions

instance HasReview Exceptions RestrictionInOutException where
    inj = SdInternalError . inj
instance HasReview Exceptions (BlockStateException Ids) where
    inj = SdInternalError . inj
instance HasReview Exceptions (StateModificationException Ids) where
    inj = SdInternalError . inj
instance HasReview Exceptions RedundantIdException where
    inj = SdInternalError . inj
instance HasReview Exceptions ValidatorExecException where
    inj = SdInternalError . inj
instance HasReview Exceptions (CSMappendException Ids) where
    inj = SdInternalError . inj
instance HasReview Exceptions TxValidationException where
    inj = SdInternalError . inj
instance HasReview Exceptions StatePException where
    inj = SdInternalError . inj
