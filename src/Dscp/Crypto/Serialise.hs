{-# LANGUAGE TypeApplications #-}

-- | Instances for binary serialisation for 'Disciplina.Crypto' datatypes

module Disciplina.Crypto.Serialise
       ( Raw
       ) where

import Universum

import Codec.Serialise (Serialise (..), serialise)
import Codec.Serialise.Decoding (decodeBytes)
import Codec.Serialise.Encoding (encodeBytes)
import Data.ByteArray (convert)
import qualified Data.ByteString.Lazy as LBS

import Disciplina.Crypto.ByteArray (FromByteArray (..))
import Disciplina.Crypto.Hash (AbstractHash (..), HasAbstractHash (..), HashFunc (..))
import Disciplina.Crypto.Signing (AbstractPK (..), AbstractSK (..), AbstractSig (..),
                                  HasAbstractSignature (..), SignatureScheme (..))

-- | 'Serialise' instance for any 'ByteArrayStruct' (including underlying
-- values of hashes, keys and signatures).
instance {-# OVERLAPPABLE #-} FromByteArray ba => Serialise ba where
    encode = encodeBytes . convert
    decode = decodeBytes >>= either fail return . fromByteArray

---------------------------------------------------------------
-- Hashes
---------------------------------------------------------------

-- | 'Serialise' instance for every 'AbstractHash' with
-- serialisable 'HashResult'.
deriving instance Serialise (HashResult hf) =>
    Serialise (AbstractHash hf a)

-- | If 'LByteString' can be hashed, then every 'Serialise' instance can.
instance {-# OVERLAPPABLE #-}
    (Serialise a, HashFunc hf, HasAbstractHash hf LByteString) =>
    HasAbstractHash hf a where
    unsafeAbstractHash = unsafeAbstractHash . serialise

----------------------------------------------------------------
-- Signatures
----------------------------------------------------------------

deriving instance Serialise (SK ss) => Serialise (AbstractSK ss)
deriving instance Serialise (PK ss) => Serialise (AbstractPK ss)
deriving instance Serialise (Sig ss) => Serialise (AbstractSig ss a)

-- If 'LByteString' can be signed/verified via signature scheme `ss`,
-- then every 'Serialise' instance can.
instance {-# OVERLAPPABLE #-}
    (Serialise a, SignatureScheme ss, HasAbstractSignature ss LByteString) =>
    HasAbstractSignature ss a where
    unsafeAbstractSign sk = unsafeAbstractSign sk . serialise
    unsafeAbstractVerify pk = unsafeAbstractVerify pk . serialise

-- | Type alias for denoting raw bytes. Indended to be used with hashes
-- and signatures, like in type `Hash Raw`, and not type-safe hashing and
-- signing.
-- TODO: probably it makes sense to make it a newtype, like in Cardano?
-- Also, is it the most appropriate place for it?
type Raw = LBS.ByteString
