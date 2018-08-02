{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}

-- | Abstract hash interface

module Dscp.Crypto.Hash.Class
       ( HashFunc (..)
       , HasAbstractHash (..)
       , AbstractHash (..)
       , abstractHash
       , hashF
       , hashLongF
       ) where

import Data.ByteArray (ByteArray, ByteArrayAccess)
import qualified Data.Text as T
import Fmt (Builder, build)

import Dscp.Crypto.ByteArray (FromByteArray)
import Dscp.Util (toHex)

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

-- | Hash builder.
hashF :: ByteArrayAccess (HashResult hf) => AbstractHash hf a -> Builder
hashF = build . T.take 8 . toHex

hashLongF :: ByteArrayAccess (HashResult hf) => AbstractHash hf a -> Builder
hashLongF = build . toHex

-- | 'GeneralizedNewtypeDeriving' cannot into type families,
-- so we do this.
deriving instance Eq (HashResult hf) => Eq (AbstractHash hf a)
deriving instance Ord (HashResult hf) => Ord (AbstractHash hf a)
deriving instance Show (HashResult hf) => Show (AbstractHash hf a)
deriving instance Monoid (HashResult hf) => Monoid (AbstractHash hf a)

deriving instance ByteArrayAccess (HashResult hf) => ByteArrayAccess (AbstractHash hf a)
deriving instance ByteArray (HashResult hf) => ByteArray (AbstractHash hf a)
deriving instance FromByteArray (HashResult hf) => FromByteArray (AbstractHash hf a)
instance Show (HashResult hf) => Buildable (AbstractHash hf a) where
    build = build @Text . show


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
