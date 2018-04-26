{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}

-- | Abstract hash interface

module Disciplina.Crypto.Hash.Class
       ( HashFunc (..)
       , HasAbstractHash (..)
       , AbstractHash (..)
       , abstractHash
       ) where

import Universum

import Data.ByteArray (ByteArrayAccess)

-- | Class of algorithms which can produce some hash value.
-- It's assumed that every hash algorithm should be able to
-- work with 'ByteArrayAccess' type.
class HashFunc hf where
    type HashResult hf :: *
    unsafeHashBytes ::
        forall a b. ByteArrayAccess a => a -> AbstractHash hf b

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

-- | For each `a`, provide a way to apply hash function `hf` to it.
-- Types with 'ByteArrayAccess' have a free pass.
-- We cannot simply provide an instance which matches all 'ByteArrayAccess'
-- instances, because it will overlap with all other instances (most
-- importantly, with instance for 'Serialise').
class HashFunc hf => HasAbstractHash hf a where
    unsafeAbstractHash ::
        forall b. a -> AbstractHash hf b
    default unsafeAbstractHash ::
        forall b. ByteArrayAccess a => a -> AbstractHash hf b
    unsafeAbstractHash = unsafeHashBytes @hf

-- | Type-safe version of 'unsafeAbstractHash'.
abstractHash :: HasAbstractHash hf a => a -> AbstractHash hf a
abstractHash = unsafeAbstractHash
