{-# LANGUAGE TypeApplications #-}

module Dscp.Witness.Instances () where

import Universum

import Codec.Serialise (Serialise (..))
import qualified Codec.Serialise as S
import Control.Monad.Free (Free (..))
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Lazy as LBS
import Data.Default (Default (..))
import Data.Hashable (Hashable (hashWithSalt))
import qualified Data.Tree.AVL as AVL

import Dscp.Crypto (HasHash, Hash, hashBytesWithSalt, unsafeHash)

-- Every 'Serialise' instance is 'Serialisable'
instance Serialise a => AVL.Serialisable a where
    serialise = LBS.toStrict . S.serialise
    deserialise = first show . S.deserialiseOrFail . LBS.fromStrict

-- 'Serialise' instances for AVL types. Temporary.
instance Serialise AVL.Tilt

instance (Serialise h, Serialise k, Serialise v, Serialise t) =>
    Serialise (AVL.MapLayer h k v t)

instance (Serialise a, Serialise (t (Free t a))) =>
    Serialise (Free t a)

instance (Serialise h, Serialise k, Serialise v) =>
    Serialise (AVL.Proof h k v)

-- AVL.Hash instance for 'Hash''
instance {-# OVERLAPPING #-} Hashable (Hash a) where
    hashWithSalt = hashBytesWithSalt

instance Default (Hash a) where
    def = unsafeHash ("" :: ByteString)

instance
  ( Show k
  , Show v
  , Ord k
  , Bounded k
  , HasHash k
  , HasHash v
  , Typeable a
  )
  => AVL.Hash (Hash a) k v
  where
    hashOf = \case
      AVL.MLBranch _ mk ck t l r -> combineAll
        [ unsafeHash mk
        , unsafeHash ck
        , unsafeHash t
        , l
        , r
        ]

      AVL.MLLeaf _ k v n p -> combineAll
        [ unsafeHash k
        , unsafeHash v
        , unsafeHash n
        , unsafeHash p
        ]

      AVL.MLEmpty _ ->
        def

combineAll :: [Hash a] -> Hash b
combineAll = unsafeHash @ByteString . BA.concat
