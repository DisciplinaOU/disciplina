module Dscp.Snowdrop.Storage.Types
    ( TxBlockRefId (..)
    , TxBlockRef (..)

    , TxData (..)
    , tdTx
    , TxsOf (..)
    , LastTx (..)
    , TxHead (..)
    , TxNext (..)

    , PublicationsOf (..)
    , LastPublication (..)
    , PublicationBlock (..)
    , PublicationHead (..)
    , PublicationNext (..)
    , PublicationData (..)
    , pdTx

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

newtype TxData = TxData
    { tdTw :: TxWitnessed
    } deriving (Eq, Ord, Show, Generic)

tdTx :: TxData -> Tx
tdTx = twTx . tdTw

instance Buildable TxData where
    build (TxData tx) = "TxData { " +| twTx tx |+ " }"

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
newtype PublicationData = PublicationData
    { pdTw   :: PublicationTxWitnessed
    } deriving (Eq, Ord, Show, Generic)

pdTx :: PublicationData -> PublicationTx
pdTx = ptwTx . pdTw

instance Buildable PublicationData where
    build pd =
        "PublicationData {" +| pdTx pd |+ " }"

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
instance Serialise TxData
instance Serialise LastTx
instance Serialise TxsOf
instance Serialise TxNext
instance Serialise TxHead
instance Serialise LastPublication
instance Serialise PublicationsOf
instance Serialise PublicationNext
instance Serialise PublicationHead
instance Serialise PublicationBlock
instance Serialise PublicationData
instance Serialise NextBlockOf
instance Serialise NextBlock
