
{-# language GeneralizedNewtypeDeriving #-}

module Disciplina.WorldState.BlakeHash (Hash) where

import Universum hiding (put, get)

import Crypto.Hash

import Data.Binary
import Data.ByteArray       as  BA
import Data.ByteString      as  BS
import Data.ByteString.Lazy as LBS (toStrict)
import Data.Default

import qualified Data.Tree.AVL as AVL

newtype Hash = Hash { getHash :: Digest Blake2sp_256 }
    deriving (Eq, Show, ByteArrayAccess)

instance Binary Hash where
    put (Hash blake) = put (BA.concat [blake] :: BS.ByteString)
    get = maybe def Hash . digestFromByteString <$> (get :: Get BS.ByteString)

instance Default Hash where
    def = Hash (hash $ BS.replicate (256 `div` 8) 0)

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
        [ hash' re
        , hash' mk
        , hash' ck
        , hash' (fromEnum t)
        , l
        , r
        ]

      AVL.MLLeaf re _ k v n p -> combineAll
        [ hash' re
        , hash' k
        , hash' v
        , hash' n
        , hash' p
        ]

      AVL.MLEmpty re _ ->
        hash' re

      AVL.MLPruned _ h _ _ _ ->
        h

hash' :: Binary b => b -> Hash
hash' b = Hash $ hash $ LBS.toStrict $ encode b

hashBA :: ByteArrayAccess b => b -> Hash
hashBA b = Hash $ hash b

combineAll :: [Hash] -> Hash
combineAll hs = hashBA (BA.concat hs :: ByteString)
