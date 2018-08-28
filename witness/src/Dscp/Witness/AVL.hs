-- | Avl+ related types and instances.

module Dscp.Witness.AVL
    ( AvlHash (..)
    , AvlProof
    ) where

import Codec.Serialise (Serialise (..))
import qualified Codec.Serialise as S
import Control.Monad.Free (Free (..))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Tree.AVL as AVL
import Snowdrop.Execution ()

import Dscp.Crypto (Hash, hash, unsafeHash)
import Dscp.Snowdrop.Configuration (Ids, Values)


-- | Hashes used in avl+ are technically not related to any data.
newtype AvlHash = AvlHash { unAvlHash :: Hash () } deriving (Eq, Ord, Show, Generic)
instance Serialise AvlHash

-- Should it be a newtype?
type AvlProof = AVL.Proof AvlHash Ids Values

instance (Serialise (f (Free f a)), Serialise a) => Serialise (Free f a)

instance Serialise AVL.Tilt
instance Serialise b => Serialise (AVL.WithBounds b)
instance (Serialise h, Serialise k, Serialise v, Serialise r) => Serialise (AVL.MapLayer h k v r)
instance (Serialise h, Serialise k, Serialise v) => Serialise (AVL.Proof h k v)
instance Serialise x => AVL.Serialisable x where
    serialise = BSL.toStrict . S.serialise
    deserialise = first show . S.deserialiseOrFail . BSL.fromStrict

instance (Serialise k, Serialise v) => AVL.Hash AvlHash k v where
    hashOf =
        AvlHash .
        (unsafeHash :: ByteString -> Hash ()) . BSL.toStrict . S.serialise
    defHash = AvlHash (hash ())
