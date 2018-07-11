{-# LANGUAGE DeriveAnyClass #-}

module Dscp.DB.SQLite.BlockData (BlockData (..)) where

import Codec.Serialise (Serialise)

import Data.Time.Clock (UTCTime)

import Dscp.Core.Types (ATGDelta)
import Dscp.Crypto (Hash, MerkleTree, MerkleSignature)
import Dscp.Educator.Txs (PrivateTx (..))
import Dscp.Educator.Block (PrivateBlock)
import Dscp.Util (HasId (..))

data BlockData = BlockData
    { _bdIndex    :: ! Word32
    , _bdTree     :: !(MerkleTree PrivateTx)
    , _bdRoot     :: !(MerkleSignature PrivateTx)
    , _bdTime     :: ! UTCTime
    , _bdPrevHash :: !(Hash PrivateBlock)
    , _bdHash     :: !(Hash PrivateBlock)
    , _bdAtgDelta :: ! ATGDelta
    }
    deriving (Eq, Show, Generic, Serialise)

instance HasId BlockData where
    type Id BlockData = Word32

    getId = _bdIndex