module Dscp.Snowdrop.Configuration where

import Control.Lens (makePrisms)
import Fmt (build, (+|))

import Snowdrop.Core (CSMappendException, ChangeSet, IdSumPrefixed (..), Prefix (..),
                      RedundantIdException, SValue, StateModificationException, StatePException,
                      StateTx (..), TxValidationException, ValidatorExecException)
import Snowdrop.Model.Block (Block (..), BlockApplicationException, BlockRef (..),
                             BlockStateException, Blund, CurrentBlockRef (..), TipKey, TipValue)
import Snowdrop.Model.State.Restrict (RestrictionInOutException)
import Snowdrop.Util (HasReview (..), IdStorage, VerifySign, WithSignature (..), deriveIdView,
                      deriveView, withInj, withInjProj)

import Dscp.Core.Foundation (HeaderHash)
import qualified Dscp.Core.Foundation as T
import Dscp.Crypto (PublicKey, Signature, hashF)
import Dscp.Snowdrop.Storage.Types
import Dscp.Snowdrop.Types (Account, AccountId (..), AccountTxTypeId (..),
                            AccountValidationException, PublicationTxTypeId (..),
                            PublicationValidationException)
import Dscp.Witness.Logic.Exceptions (LogicException)

----------------------------------------------------------------------------
-- Snowdrop block-related types
----------------------------------------------------------------------------

type SHeader  = T.Header
-- We store payload twice b/c snowdrop can't into blocks. This allows
-- us to reconstruct block back from SDBlock. DSCP-175
data SPayload = SPayload { sPayStateTxs :: [StateTx Ids Proofs Values]
                         , sPayOrigBody :: T.BlockBody
                         } deriving (Eq, Show, Generic)
type SBlock   = Block SHeader SPayload
type SUndo    = ChangeSet Ids Values
type SBlund   = Blund SHeader SPayload SUndo

sBlockReconstruct :: SBlock -> T.Block
sBlockReconstruct (Block h (SPayload _ b)) = T.Block h b

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
    | TxVal              T.GTxWitnessed
    | TxOfVal            LastTx
    | TxHeadVal          TxNext
    | PublicationOfVal   LastPublication
    | PublicationHeadVal PublicationNext
    deriving (Eq, Show, Generic)

type instance SValue  TipKey               = TipValue HeaderHash
type instance SValue (BlockRef HeaderHash) = SBlund
type instance SValue  AccountId            = Account
type instance SValue  T.GTxId              = T.GTxWitnessed
type instance SValue  TxsOf                = LastTx
type instance SValue  TxHead               = TxNext
type instance SValue  PublicationsOf       = LastPublication
type instance SValue  PublicationHead      = PublicationNext

----------------------------------------------------------------------------
-- Proofs
----------------------------------------------------------------------------

deriving instance (Eq   pk, Eq   sig, Eq   a) => Eq   (WithSignature pk sig a)
deriving instance (Show pk, Show sig, Show a) => Show (WithSignature pk sig a)

type CanVerifyPayload txid payload =
    VerifySign
        PublicKey
        (Signature
            (txid, PublicKey, payload))
        (txid, PublicKey, payload)


type PersonalisedProof txid  payload =
    WithSignature
        PublicKey
        (Signature (txid, PublicKey, payload))
        (txid, PublicKey, payload)

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
