module Dscp.Snowdrop.Configuration where

import Fmt (build, (+|))

import Snowdrop.Model.Block (Block, BlockRef (..), BlockStateException, Blund, TipKey, TipValue)
import Snowdrop.Model.State.Accounting.Account (Account)
import Snowdrop.Model.State.Core (SValue, StatePException, StateTx (..))
import Snowdrop.Model.State.Restrict (RestrictionInOutException)
import Snowdrop.Util (CSMappendException, ChangeSet, IdStorage, IdSumPrefixed (..), Prefix (..),
                      WithSignature (..), deriveIdView, deriveView, withInj, withInjProj)

import Dscp.Core (Address, HeaderHash)
import qualified Dscp.Core.Types as T
import Dscp.Crypto (PublicKey, Signature, hashF)

----------------------------------------------------------------------------
-- Snowdrop block-related types
----------------------------------------------------------------------------

type SHeader = T.Header
type SPayload = [StateTx Ids Proofs Values]
type SBlock = Block SHeader SPayload
type SUndo = ChangeSet Ids Values
type SBlund = Blund SHeader SPayload SUndo

----------------------------------------------------------------------------
-- Identities/prefixes
----------------------------------------------------------------------------

tipPrefix :: Prefix
tipPrefix = Prefix 1

blockPrefix :: Prefix
blockPrefix = Prefix 2

accountPrefix :: Prefix
accountPrefix = Prefix 3

-- | Sum-type for all ids used within the application.
data Ids
    = TipKeyIds TipKey
    | BlockRefIds (BlockRef HeaderHash)
    | AccountInIds Address
    deriving (Eq, Ord, Show, Generic)

instance Buildable Ids where
    build = ("Key" <>) . \case
        TipKeyIds t -> build t
        BlockRefIds (BlockRef r) -> "block ref " +| hashF r
        AccountInIds a -> build a

instance IdSumPrefixed Ids where
    idSumPrefix (TipKeyIds _)    = tipPrefix
    idSumPrefix (BlockRefIds _)  = blockPrefix
    idSumPrefix (AccountInIds _) = accountPrefix

----------------------------------------------------------------------------
-- Values
----------------------------------------------------------------------------

data Values
    = TipValueVal (TipValue HeaderHash)
    | BlundVal SBlund
    | AccountOutVal Account
    deriving (Eq, Show, Generic)

type instance SValue TipKey = TipValue HeaderHash
type instance SValue (BlockRef HeaderHash) = SBlund
type instance SValue Address = Account

----------------------------------------------------------------------------
-- Proofs
----------------------------------------------------------------------------

deriving instance (Eq pk, Eq sig, Eq a) => Eq (WithSignature pk sig a)
deriving instance (Show pk, Show sig, Show a) => Show (WithSignature pk sig a)

type AddrTxProof =
    WithSignature PublicKey
                  (Signature (T.TxId, T.TxInAcc))
                  (T.TxId, T.TxInAcc)

data Proofs =
    AddressTxWitness AddrTxProof
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
    = ExpanderRestrictionError RestrictionInOutException
    | BlockStateError (BlockStateException Ids)
    | CSMappendError (CSMappendException Ids)
    | StatePError StatePException
    | ExpanderError ExpanderException
    deriving (Show)

instance Exception Exceptions

----------------------------------------------------------------------------
-- TxIds
----------------------------------------------------------------------------

data MoneyTxId = MoneyTxId deriving (Eq,Show,Enum)

data TxIds = MoneyTxIds MoneyTxId deriving (Eq,Show)

instance Enum TxIds where
    toEnum = MoneyTxIds . toEnum
    fromEnum (MoneyTxIds MoneyTxId) = 0

instance IdStorage TxIds MoneyTxId

----------------------------------------------------------------------------
-- HasReview
----------------------------------------------------------------------------

deriveView withInjProj ''Ids
deriveIdView withInjProj ''Ids

deriveView withInjProj ''Values
deriveIdView withInjProj ''Values


deriveView withInjProj ''TxIds
deriveView withInj ''Exceptions
