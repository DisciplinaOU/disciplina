{-# LANGUAGE TypeApplications #-}

-- | Instances for binary serialisation for 'Disciplina.Crypto' datatypes

module Disciplina.Crypto.Serialise () where

import Universum

import Codec.Serialise (Serialise (..), serialise)
import Codec.Serialise.Decoding (decodeBytes)
import Codec.Serialise.Encoding (encodeBytes)
import Data.ByteArray (convert)

import Disciplina.Crypto.ByteArray (ByteArrayStruct (..))
import Disciplina.Crypto.Hash (AbstractHash (..), CryptoniteFunc, HasAbstractHash (..),
                               HashFunc (..))
import Disciplina.Crypto.Signing (AbstractPK (..), AbstractSK (..), AbstractSig (..),
                                  HasAbstractSignature (..), SignatureScheme (..))

-- | 'Serialise' instance for any 'ByteArrayStruct' (including underlying
-- values of hashes, keys and signatures).
instance {-# OVERLAPPABLE #-} ByteArrayStruct ba => Serialise ba where
    encode = encodeBytes . convert
    decode = decodeBytes >>= either fail return . reconstruct

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
