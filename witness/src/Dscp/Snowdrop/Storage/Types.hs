module Dscp.Snowdrop.Storage.Types
    ( TxsOf (..)
    , LastTx (..)
    , TxHead (..)
    , TxNext (..)

    , PublicationsOf (..)
    , LastPublication (..)
    , PublicationHead (..)
    , PublicationNext (..)
    ) where

import Codec.Serialise (Serialise (..))
import Data.Text.Buildable (Buildable (..))
import Fmt ((+|), (|+))

import Dscp.Core.Foundation

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
data LastPublication
    = LastPublication PrivateHeaderHash
    deriving (Eq, Ord, Show, Generic)

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

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

instance Serialise LastTx
instance Serialise TxsOf
instance Serialise TxNext
instance Serialise TxHead
instance Serialise LastPublication
instance Serialise PublicationsOf
instance Serialise PublicationNext
instance Serialise PublicationHead