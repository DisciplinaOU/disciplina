{-# LANGUAGE DeriveAnyClass #-}

module Dscp.DB.SQLite.BlockData (BlockData (..), TxInBlock (..), TxWithIdx (..)) where

import Codec.Serialise (Serialise)

import Data.Time.Clock (UTCTime)

import Dscp.Core.Foundation.Educator.Block (PrivateBlock)
import Dscp.Core.Foundation.Educator.Txs (PrivateTx (..))
import Dscp.Core.Types (ATGDelta)
import Dscp.Crypto (EmptyMerkleTree, Hash, MerkleSignature)
import Dscp.Util (HasId (..))

data BlockData = BlockData
    { _bdIndex    :: ! Word32
    , _bdHash     :: !(Hash PrivateBlock)
    , _bdTime     :: ! UTCTime
    , _bdPrevHash :: !(Hash PrivateBlock)
    , _bdAtgDelta :: ! ATGDelta
    , _bdRoot     :: !(MerkleSignature PrivateTx)
    , _bdTree     :: !(EmptyMerkleTree PrivateTx)
    }
    deriving (Eq, Show, Generic, Serialise)

data TxInBlock = TxInBlock
    { _tibTx      :: TxWithIdx
    , _tibBlockId :: Word32
    }
    deriving (Eq, Show, Generic, Serialise)

data TxWithIdx = TxWithIdx
    { _twiTx    :: PrivateTx
    , _twiTxIdx :: Word32
    }
    deriving (Eq, Show, Generic, Serialise)

instance HasId BlockData where
    type Id BlockData = Word32

    getId = _bdIndex
