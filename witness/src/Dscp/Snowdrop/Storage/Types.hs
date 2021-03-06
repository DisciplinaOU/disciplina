module Dscp.Snowdrop.Storage.Types
    ( TxBlockRefId (..)
    , TxBlockRef (..)

    , TxItself (..)
    , tiTx
    , TxsOf (..)
    , LastTx (..)
    , TxHead (..)
    , TxNext (..)

    , PublicationsOf (..)
    , LastPublication (..)
    , PublicationBlock (..)
    , PublicationHead (..)
    , PublicationNext (..)
    , PublicationItself (..)
    , piTx

    , NextBlockOf (..)
    , NextBlock (..)
    ) where

import Codec.Serialise (Serialise (..))
import Data.Text.Buildable (Buildable (..))
import Fmt ((+|), (|+))

import Dscp.Core.Foundation

newtype TxBlockRefId = TxBlockRefId GTxId
    deriving (Eq, Ord, Show, Generic)

instance Buildable TxBlockRefId where
    build (TxBlockRefId txId) = "TxBlockRefId { " +| txId |+ " }"

-- | Transaction position in blockchain
data TxBlockRef = TxBlockRef
    { tbrBlockRef :: HeaderHash
    , tbrTxIdx    :: Int  -- ^ Zero-based index of tx in the block
    }
    deriving (Eq, Ord, Show, Generic)

newtype TxItself = TxItself
    { tiTw :: TxWitnessed
    } deriving (Eq, Ord, Show, Generic)

tiTx :: TxItself -> Tx
tiTx = twTx . tiTw

instance Buildable TxItself where
    build (TxItself tx) = "TxItself { " +| twTx tx |+ " }"

-- | Account transaction linked-list storage structure.
-- |
-- | For each account, we'll have a `TxsOf address ~> LastTx txId`
-- | (which is effectively `(account, lastTxId)`)
-- | to determine the last transaction.
newtype TxsOf = TxsOf Address
    deriving (Eq, Ord, Show, Generic)

instance Buildable TxsOf where
    build (TxsOf addr) = "TxsOf { " +| addr |+  " }"

-- | Points to the `TxHead addr` in the database.
data LastTx = LastTx { unLastTx :: GTxId }
    deriving (Eq, Ord, Show, Generic)

-- | Once 'LastTx' is known, you can walk the chain of
-- | `TxHead address txId ~> TxNext txId`
-- | (which is `(txId, Maybe txId)`)
data TxHead = TxHead Address GTxId
    deriving (Eq, Ord, Show, Generic)

instance Buildable TxHead where
    build (TxHead addr txId) = "TxHead { " +| addr |+  ", " +| txId |+  " }"

data TxNext = TxNext { unTxNext :: GTxId }
    deriving (Eq, Ord, Show, Generic)

-- | Points to public block which contains given publication.
newtype PublicationBlock = PublicationBlock
    { unPublicationBlock :: PublicationTxId
    } deriving (Eq, Ord, Show, Generic)

instance Buildable PublicationBlock where
    build (PublicationBlock h) =
        "PublicationBlock { " +| h |+ " }"

-- | Educator publication linked-list storage structure.
-- |
-- | For each educator, we'll have a `PublicationOf id ~> LastPublication blockHash`
-- | (which is effectively `(educator, lastBlockHash)`)
-- | to determine the lst publication.
newtype PublicationsOf
    = PublicationsOf Address
    deriving (Eq, Ord, Show, Generic)

instance Buildable PublicationsOf where
    build (PublicationsOf addr) =
        "PublicationsOf { " +| addr |+  " }"

-- | Points to the `PublicationHead addr` in the database.
newtype LastPublication = LastPublication
    { unLastPublication :: PrivateHeaderHash
    } deriving (Eq, Ord, Show, Generic)

-- | Once 'LastPublication' is known, you can walk the chain of
-- | `PublicationHead bh ~> PublicationNext bh`,
-- | (which is `(blockHash, Maybe blockHash)`)
-- | where phead contains block hash and pnext has prev block hash.
newtype PublicationHead
    = PublicationHead PrivateHeaderHash
    deriving (Eq, Ord, Show, Generic)

instance Buildable PublicationHead where
    build (PublicationHead blk) =
        "PublicationHead { " +| blk |+  " }"

data PublicationNext
    = PublicationNext (Maybe PrivateHeaderHash)
    deriving (Eq, Ord, Show, Generic)

-- | Publication taken by id.
newtype PublicationItself = PublicationItself
    { piTw   :: PublicationTxWitnessed
    } deriving (Eq, Ord, Show, Generic)

piTx :: PublicationItself -> PublicationTx
piTx = ptwTx . piTw

instance Buildable PublicationItself where
    build pd =
        "PublicationItself {" +| piTx pd |+ " }"

-- | Key/value types for nextBlock chain storage
newtype NextBlockOf = NextBlockOf { unNextBlockOf :: HeaderHash }
    deriving (Eq, Ord, Show, Buildable, Generic)

newtype NextBlock = NextBlock { unNextBlock :: HeaderHash }
    deriving (Eq, Ord, Show, Buildable, Generic)

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

instance Serialise TxBlockRefId
instance Serialise TxBlockRef
instance Serialise TxItself
instance Serialise LastTx
instance Serialise TxsOf
instance Serialise TxNext
instance Serialise TxHead
instance Serialise LastPublication
instance Serialise PublicationsOf
instance Serialise PublicationNext
instance Serialise PublicationHead
instance Serialise PublicationBlock
instance Serialise PublicationItself
instance Serialise NextBlockOf
instance Serialise NextBlock
