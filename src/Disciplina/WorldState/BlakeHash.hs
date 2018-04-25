
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Disciplina.WorldState.BlakeHash where

import qualified Prelude (show)
import Universum hiding (Hashable, get, put)

import Codec.Serialise (Serialise (..), serialise)
import Crypto.Hash (Blake2sp_256, Digest, digestFromByteString)
import qualified Crypto.Hash as Base (hash)
import Data.ByteArray as BA
import Data.ByteString.Lazy as LBS (toStrict)
import Data.Default

import qualified Data.Hashable as StdHash (Hashable (hash, hashWithSalt))

import qualified Data.Tree.AVL as AVL

class HasHash a where
    hash :: a -> Hash

newtype Hash = Hash { getHash :: Digest Blake2sp_256 }
    deriving (Eq, Ord, ByteArrayAccess)

instance Serialise Hash where
    decode = do
      bs <- decode
      maybe (fail "get @(Hash): not a Blake2sp_256") (return . Hash) $
        digestFromByteString (bs :: ByteString)

    encode = encode . (convert :: Digest Blake2sp_256 -> ByteString) . getHash

instance HasHash Hash where
    hash = identity

instance Show Hash where
    show (Hash raw) = "#" <> Universum.take 8 (show raw)

instance Default Hash where
    def = hash ()

instance StdHash.Hashable Hash where
    hash              = StdHash.hash              . (convert :: Hash -> ByteString)
    hashWithSalt salt = StdHash.hashWithSalt salt . (convert :: Hash -> ByteString)

instance
  ( Show k
  , Show v
  , Eq k
  , Eq v
  , Ord k
  , Bounded k
  , Serialise k
  , Serialise v
  , StdHash.Hashable Hash
  )
  => AVL.Hash Hash k v
  where
    hashOf = \case
      AVL.MLBranch _ mk ck t l r -> combineAll
        [ hash mk
        , hash ck
        , hash (fromEnum t)
        , l
        , r
        ]

      AVL.MLLeaf _ k v n p -> combineAll
        [ hash k
        , hash v
        , hash n
        , hash p
        ]

      AVL.MLEmpty _ ->
        def

instance Serialise b => HasHash b where
    hash = Hash . Base.hash . LBS.toStrict . serialise

hashBA :: ByteString -> Hash
hashBA = Hash . Base.hash

combineAll :: [Hash] -> Hash
combineAll hs = hashBA (BA.concat hs :: ByteString)
