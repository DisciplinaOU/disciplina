
{-# language GeneralizedNewtypeDeriving #-}

module Disciplina.WorldState.BlakeHash where

import Universum hiding (put, get, Hashable)

import qualified Crypto.Hash as Base (hash)
import Crypto.Hash (Digest, digestFromByteString, Blake2sp_256)

import Data.Binary
import Data.ByteArray       as  BA
import Data.ByteString      as  BS
import Data.ByteString.Lazy as LBS (toStrict)
import Data.Default

import qualified Data.Tree.AVL as AVL

class Hashable a where
    hash :: a -> Hash

newtype Hash = Hash { getHash :: Digest Blake2sp_256 }
    deriving (Eq, Ord, Show, ByteArrayAccess)

instance Binary Hash where
    put (Hash blake) = put (BA.concat [blake] :: BS.ByteString)
    get = maybe def Hash . digestFromByteString <$> (get :: Get BS.ByteString)

instance Default Hash where
    def = hash ()

instance
  ( Show k
  , Show v
  , Eq k
  , Eq v
  , Ord k
  , Bounded k
  , Binary k
  , Binary v
  )
  => AVL.Hash Hash k v
  where
    hashOf = \case
      AVL.MLBranch re _ mk ck t l r -> combineAll
        [ hash re
        , hash mk
        , hash ck
        , hash (fromEnum t)
        , l
        , r
        ]

      AVL.MLLeaf re _ k v n p -> combineAll
        [ hash re
        , hash k
        , hash v
        , hash n
        , hash p
        ]

      AVL.MLEmpty re _ ->
        hash re

      AVL.MLPruned _ h _ _ _ ->
        h

instance Binary b => Hashable b where
    hash = Hash . Base.hash . LBS.toStrict . encode

hashBA :: ByteString -> Hash
hashBA = Hash . Base.hash

combineAll :: [Hash] -> Hash
combineAll hs = hashBA (BA.concat hs :: ByteString)
