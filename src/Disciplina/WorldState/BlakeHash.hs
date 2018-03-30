
{-# language GeneralizedNewtypeDeriving #-}

module Disciplina.WorldState.BlakeHash where

import qualified Prelude (show)
import Universum hiding (put, get, Hashable)

import qualified Crypto.Hash as Base (hash)
import Crypto.Hash (Digest, digestFromByteString, Blake2sp_256)

import Data.Binary
import Data.ByteArray       as  BA
import Data.ByteString      as  BS
import Data.ByteString.Lazy as LBS (toStrict)
import Data.Default

import qualified Data.Hashable as StdHash (Hashable(hash, hashWithSalt))

import qualified Data.Tree.AVL as AVL

class HasHash a where
    hash :: a -> Hash

newtype Hash = Hash { getHash :: Digest Blake2sp_256 }
    deriving (Eq, Ord, ByteArrayAccess)

instance Binary Hash where
    get = do
      bs <- get
      maybe (fail "get @(Hash): not a Blake2sp_256") (return . Hash) $
        digestFromByteString (bs :: ByteString)

    put = put . (convert :: Digest Blake2sp_256 -> ByteString) . getHash

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
  , Binary k
  , Binary v
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

instance Binary b => HasHash b where
    hash = Hash . Base.hash . LBS.toStrict . encode

hashBA :: ByteString -> Hash
hashBA = Hash . Base.hash

combineAll :: [Hash] -> Hash
combineAll hs = hashBA (BA.concat hs :: ByteString)
