{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Abstract hash interface

module Disciplina.Crypto.Hash.Class
       ( HashFunc (..)
       , HasAbstractHash (..)
       , AbstractHash (..)
       , abstractHash
       ) where

import Universum

-- | Class of algorithms which can produce some hash value.
class HashFunc hf where
    type HashResult hf :: *

-- | Wrapper for a hash value. Phantom type parameter 'a' denotes
-- the type of object being hashed.
newtype AbstractHash hf a = AbstractHash (HashResult hf)

-- | 'GeneralizedNewtypeDeriving' cannot into type families,
-- so we do this.
deriving instance Eq (HashResult hf) => Eq (AbstractHash hf a)
deriving instance Ord (HashResult hf) => Ord (AbstractHash hf a)
deriving instance Show (HashResult hf) => Show (AbstractHash hf a)

-- | For each `a`, provide a way to apply hash function `hf` to it.
class HashFunc hf => HasAbstractHash hf a where
    unsafeAbstractHash ::
        forall b. a -> AbstractHash hf b

-- | Type-safe version of
abstractHash :: HasAbstractHash hf a => a -> AbstractHash hf a
abstractHash = unsafeAbstractHash
