module Dscp.Snowdrop.Configuration where

import Control.Lens (makePrisms)
import Fmt (build, (+|))

import Snowdrop.Block (Block (..), BlockApplicationException, BlockRef (..), BlockStateException,
                       Blund (buBlock), CurrentBlockRef (..), TipKey, TipValue)
import Snowdrop.Core (CSMappendException, IdSumPrefixed (..), Prefix (..), RedundantIdException,
                      SValue, StateModificationException, StatePException, StateTx (..),
                      TxValidationException, Undo, ValidatorExecException)
import Snowdrop.Execution (RestrictionInOutException)
import Snowdrop.Util (HasReview (..), IdStorage, VerifySign, WithSignature (..), deriveIdView,
                      deriveView, withInj, withInjProj, verifySignature)
import qualified Snowdrop.Util as SD (PublicKey, Signature)

import Dscp.Core.Foundation (HeaderHash)
import qualified Dscp.Core.Foundation as T
import Dscp.Crypto (Hash, PublicKey, hashF, Signature, PublicKey, HasAbstractSignature,
                    verify, SigScheme)
import Dscp.Snowdrop.Storage.Types
import Dscp.Snowdrop.Types (Account, AccountId (..), AccountTxTypeId (..),
                            AccountValidationException, PublicationTxTypeId (..),
                            PublicationValidationException)
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

-- | Sum-type for all ids used within the application.
data Ids
    = TipKeyIds          TipKey
    | BlockRefIds       (BlockRef  HeaderHash)
    | AccountInIds       AccountId
    | TxIds              T.GTxId
    | TxOfIds            TxsOf
    | TxHeadIds          TxHead
    | PublicationOfIds   PublicationsOf
    | PublicationHeadIds PublicationHead
    deriving (Eq, Ord, Show, Generic)

instance Buildable Ids where
    build = ("Key " <>) . \case
        TipKeyIds          t            -> build t
        BlockRefIds       (BlockRef  r) -> "block ref " +| hashF r
        AccountInIds      (AccountId a) -> build a
        TxIds              gTxId        -> build gTxId
        TxOfIds            t            -> build t
        TxHeadIds          th           -> build th
        PublicationOfIds   p            -> build p
        PublicationHeadIds ph           -> build ph

instance IdSumPrefixed Ids where
    idSumPrefix (TipKeyIds          _) = tipPrefix
    idSumPrefix (BlockRefIds        _) = blockPrefix
    idSumPrefix (AccountInIds       _) = accountPrefix
    idSumPrefix (TxIds              _) = txPrefix
    idSumPrefix (TxOfIds            _) = txOfPrefix
    idSumPrefix (TxHeadIds          _) = txHeadPrefix
    idSumPrefix (PublicationOfIds   _) = publicationOfPrefix
    idSumPrefix (PublicationHeadIds _) = publicationHeadPrefix

instance HasReview Ids (BlockRef (CurrentBlockRef HeaderHash)) where
    inj (BlockRef (CurrentBlockRef h)) = BlockRefIds (BlockRef h)

----------------------------------------------------------------------------
-- Values
----------------------------------------------------------------------------

data Values
    = TipValueVal       (TipValue HeaderHash)
    | BlundVal           SBlund
    | AccountOutVal      Account
    | TxVal              TxBlockRef
    | TxOfVal            LastTx
    | TxHeadVal          TxNext
    | PublicationOfVal   LastPublication
    | PublicationHeadVal PublicationNext
    deriving (Eq, Show, Generic)

type instance SValue  TipKey               = TipValue HeaderHash
type instance SValue (BlockRef HeaderHash) = SBlund
type instance SValue  AccountId            = Account
type instance SValue  T.GTxId              = TxBlockRef
type instance SValue  TxsOf                = LastTx
type instance SValue  TxHead               = TxNext
type instance SValue  PublicationsOf       = LastPublication
type instance SValue  PublicationHead      = PublicationNext

----------------------------------------------------------------------------
-- Proofs
----------------------------------------------------------------------------

deriving instance (Eq (SD.Signature sigScheme a), Eq (SD.PublicKey sigScheme), Eq a) => Eq (WithSignature sigScheme a)
deriving instance (Show (SD.Signature sigScheme a), Show (SD.PublicKey sigScheme), Show a) => Show (WithSignature sigScheme a)

type CanVerifyPayload txid payload =
    VerifySign DscpSigScheme (txid, PublicKey, payload)

type PersonalisedProof txid payload =
    WithSignature DscpSigScheme (txid, PublicKey, payload)

type AddrTxProof =
    PersonalisedProof T.TxId ()

type PublicationTxProof =
    PersonalisedProof T.PublicationTxId T.Publication

data Proofs
    = AddressTxWitness     AddrTxProof         -- ^ Money transaction witness
    | PublicationTxWitness PublicationTxProof  -- ^ Publication transaction witness
    deriving (Eq, Show, Generic)

----------------------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------------------

data ExpanderException =
    MTxDuplicateOutputs
    | CantResolveSender
    deriving (Show)

data Exceptions
    = ExpanderRestrictionError   RestrictionInOutException
    | BlockStateError           (BlockStateException        Ids)
    | BlockApplicationError     (BlockApplicationException  HeaderHash)
    | StateModificationError    (StateModificationException Ids)
    | AccountValidationError     AccountValidationException
    | PublicationValidationError PublicationValidationException
    | RedundantIdError           RedundantIdException
    | ValidatorExecError         ValidatorExecException
    | CSMappendError            (CSMappendException         Ids)
    | TxValidationError          TxValidationException
    | StatePError                StatePException
    | ExpanderError              ExpanderException
    | LogicError                 LogicException
    deriving (Show)

instance Exception Exceptions

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
-- HasReview and lenses
----------------------------------------------------------------------------

makePrisms ''Values

deriveView withInjProj ''Ids
deriveIdView withInjProj ''Ids

deriveView withInjProj ''Values
deriveIdView withInjProj ''Values

deriveView withInjProj ''TxIds
deriveView withInj ''Exceptions
