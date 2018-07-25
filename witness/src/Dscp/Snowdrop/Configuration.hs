module Dscp.Snowdrop.Configuration where

import Control.Lens (makePrisms)
import Fmt (build, (+|))

import Snowdrop.Model.Block (Block, BlockApplicationException, BlockRef (..), BlockStateException,
                             Blund, TipKey, TipValue)
import Snowdrop.Model.State.Core (RedundantIdException, SValue, StateModificationException,
                                  StatePException, StateTx (..), TxValidationException,
                                  ValidatorExecException)
import Snowdrop.Model.State.Restrict (RestrictionInOutException)
import Snowdrop.Util (CSMappendException, ChangeSet, IdStorage, IdSumPrefixed (..), Prefix (..),
                      WithSignature (..), deriveIdView, deriveView, withInj, withInjProj, VerifySign)

import Dscp.Core.Foundation.Transactions (HeaderHash)
import qualified Dscp.Core.Foundation.Transactions as T
import Dscp.Crypto (PublicKey, Signature, hashF)
import Dscp.Snowdrop.Types (Account, AccountId (..), AccountTxTypeId (..), AccountValidationException, PublicationTxTypeId (..), PublicationValidationError)

----------------------------------------------------------------------------
-- Snowdrop block-related types
----------------------------------------------------------------------------

type SHeader  = T.Header
type SPayload = [StateTx Ids Proofs Values]
type SBlock   = Block SHeader SPayload
type SUndo    = ChangeSet Ids Values
type SBlund   = Blund SHeader SPayload SUndo

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

-- | Sum-type for all ids used within the application.
data Ids
    = TipKeyIds          TipKey
    | BlockRefIds       (BlockRef  HeaderHash)
    | AccountInIds       AccountId
    | PublicationOfIds   T.PublicationsOf
    | PublicationHeadIds T.PublicationHead
    deriving (Eq, Ord, Show, Generic)

instance Buildable Ids where
    build = ("Key " <>) . \case
        TipKeyIds          t            -> build t
        BlockRefIds       (BlockRef  r) -> "block ref " +| hashF r
        AccountInIds      (AccountId a) -> build a
        PublicationOfIds   p            -> build p
        PublicationHeadIds ph           -> build ph

instance IdSumPrefixed Ids where
    idSumPrefix (TipKeyIds          _) = tipPrefix
    idSumPrefix (BlockRefIds        _) = blockPrefix
    idSumPrefix (AccountInIds       _) = accountPrefix
    idSumPrefix (PublicationOfIds   _) = publicationOfPrefix
    idSumPrefix (PublicationHeadIds _) = publicationHeadPrefix

----------------------------------------------------------------------------
-- Values
----------------------------------------------------------------------------

data Values
    = TipValueVal       (TipValue HeaderHash)
    | BlundVal           SBlund
    | AccountOutVal      Account
    | PublicationOfVal   T.LastPublication
    | PublicationHeadVal T.PublicationNext
    deriving (Eq, Show, Generic)

type instance SValue  TipKey               = TipValue HeaderHash
type instance SValue (BlockRef HeaderHash) = SBlund
type instance SValue  AccountId            = Account
type instance SValue  T.PublicationsOf     = T.LastPublication
type instance SValue  T.PublicationHead    = T.PublicationNext

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
    = AddressTxWitness     AddrTxProof
    | PublicationTxWitness PublicationTxProof
    -- ^ Money transaction witness
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
    | PublicationValidationError PublicationValidationError
    | RedundantIdError           RedundantIdException
    | ValidatorExecError         ValidatorExecException
    | CSMappendError            (CSMappendException         Ids)
    | TxValidationError          TxValidationException
    | StatePError                StatePException
    | ExpanderError              ExpanderException
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
