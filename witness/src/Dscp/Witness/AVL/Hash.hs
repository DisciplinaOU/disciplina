-- | Avl+ related types and instances.

module Dscp.Witness.AVL.Hash
    ( AvlHash (..)
    ) where

import Codec.Serialise (Serialise (..))
import qualified Codec.Serialise as S
import Control.Monad.Free (Free (..))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Tree.AVL as AVL
import Snowdrop.Execution ()

import Dscp.Crypto (Hash, unsafeHash)

-- | Hashes used in avl+ are technically not related to any data.
newtype AvlHash = AvlHash { unAvlHash :: Hash () } deriving (Eq, Ord, Show, Generic)
instance Serialise AvlHash

instance (Serialise (f (Free f a)), Serialise a) => Serialise (Free f a)

instance Serialise AVL.Tilt
instance Serialise b => Serialise (AVL.WithBounds b)
instance (Serialise h, Serialise k, Serialise v, Serialise r) => Serialise (AVL.MapLayer h k v r)

instance (Serialise k, Serialise v) => AVL.Hash AvlHash k v where
    hashOf =
        AvlHash .
        (unsafeHash :: ByteString -> Hash ()) . BSL.toStrict . S.serialise
