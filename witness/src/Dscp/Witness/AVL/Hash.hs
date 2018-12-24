-- | Avl+ related types and instances.

module Dscp.Witness.AVL.Hash
    ( AvlHash (..)
    ) where

import Codec.Serialise (Serialise (..), serialise)
import Control.Monad.Free (Free (..))
import qualified Data.Tree.AVL as AVL
import qualified Data.ByteString.Lazy as BSL
import Snowdrop.Execution ()

import Dscp.Crypto (Hash, unsafeHash)

-- | Hashes used in avl+ are technically not related to any data.
newtype AvlHash = AvlHash { unAvlHash :: Hash () } deriving (Eq, Ord, Show, Generic)
instance Serialise AvlHash

instance {-# OVERLAPPING #-} (Serialise h, Serialise k, Serialise v) => Serialise (AVL.Map h k v) where
    encode = encode . AVL.beforeSerialise
    decode = AVL.afterDeserialise <$> decode

instance (Serialise a, Serialise (f (Free f a))) => Serialise (Free f a)

-- instance Serialise AVL.Tilt
-- instance Serialise b => Serialise (AVL.WithBounds b)
instance (Serialise h, Serialise k, Serialise v, Serialise r, Serialise t) => Serialise (AVL.MapLayerTemplate t h k v r)

instance Serialise x => AVL.ProvidesHash x AvlHash where
    getHash
        = AvlHash
        . (unsafeHash :: ByteString -> Hash ())
        . BSL.toStrict
        . serialise
