{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Abstract hash interface

module Disciplina.Crypto.Hash.Class
       ( HashFunction (..)
       , AbstractHash (..)
       , abstractHash
       ) where

import Universum

import Codec.Serialise (Serialise)
import Data.ByteArray (ByteArrayAccess)

-- | Wrapper for a hash value. Phantom type parameter 'a' denotes
-- the type of object being hashed.
newtype AbstractHash hf a = AbstractHash (HashResult hf)

-- | 'GeneralizedNewtypeDeriving' cannot into type families,
-- so we do this.
deriving instance Eq (HashResult hf) => Eq (AbstractHash hf a)
deriving instance Ord (HashResult hf) => Ord (AbstractHash hf a)
deriving instance Show (HashResult hf) => Show (AbstractHash hf a)
deriving instance ByteArrayAccess (HashResult hf) =>
    ByteArrayAccess (AbstractHash hf a)
deriving instance Serialise (HashResult hf) =>
    Serialise (AbstractHash hf a)

class HashFunction hf where
    type HashResult hf :: *

    unsafeAbstractHash ::
        forall a b. ByteArrayAccess a => a -> AbstractHash hf b

-- | Type-safe version of
abstractHash ::
       forall hf a. (HashFunction hf, ByteArrayAccess a)
    => a -> AbstractHash hf a
abstractHash = unsafeAbstractHash
