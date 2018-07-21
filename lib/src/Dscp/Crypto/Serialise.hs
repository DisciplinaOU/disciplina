{-# LANGUAGE TypeApplications #-}

-- | Instances for binary serialisation for 'Dscp.Crypto' datatypes
--
-- We provide them for every possible framework, but expect serialisee
-- instances to come from 'FromByteArray'.

module Dscp.Crypto.Serialise
       ( Raw
       ) where

import Codec.Serialise (Serialise (..), serialise)
import Codec.Serialise.Decoding (Decoder, decodeBytes)
import Codec.Serialise.Encoding (Encoding, encodeBytes)
import Data.ByteArray (ByteArrayAccess, convert)
import qualified Data.ByteString.Lazy as LBS

import Dscp.Crypto.ByteArray (FromByteArray (..))
import Dscp.Crypto.Hash (AbstractHash (..), HasAbstractHash (..), HashFunc (..))
import Dscp.Crypto.Impl (Signed (..))
import Dscp.Crypto.Signing (AbstractPK (..), AbstractSK (..), AbstractSig (..),
                            HasAbstractSignature (..), SignatureScheme (..))

---------------------------------------------------------------
-- Hashes
---------------------------------------------------------------

instance FromByteArray (AbstractHash hf a) => Serialise (AbstractHash hf a) where
    encode = encodeBA
    decode = decodeBA

-- | If 'LByteString' can be hashed, then every 'Serialise' instance can.
instance {-# OVERLAPPABLE #-}
    (Serialise a, HashFunc hf, HasAbstractHash hf LByteString) =>
        HasAbstractHash hf a where
    unsafeAbstractHash = unsafeAbstractHash . serialise

----------------------------------------------------------------
-- Signatures
----------------------------------------------------------------

instance FromByteArray (AbstractSK ss) => Serialise (AbstractSK ss) where
    encode = encodeBA
    decode = decodeBA

instance FromByteArray (AbstractPK ss) => Serialise (AbstractPK ss) where
    encode = encodeBA
    decode = decodeBA

instance FromByteArray (AbstractSig ss a) => Serialise (AbstractSig ss a) where
    encode = encodeBA
    decode = decodeBA

instance Serialise msg => Serialise (Signed msg)

-- If 'LByteString' can be signed/verified via signature scheme `ss`,
-- then every 'Serialise' instance can.
--
-- This is a "base" instance, it's supposed to be overlapped with
-- standalone instances like "HasAbstractSignature ss ByteString"
-- etc.
instance {-# OVERLAPPABLE #-}
    (Serialise a, SignatureScheme ss, HasAbstractSignature ss LByteString) =>
        HasAbstractSignature ss a where
    unsafeAbstractSign sk = unsafeAbstractSign sk . serialise
    unsafeAbstractVerify pk = unsafeAbstractVerify pk . serialise

----------------------------------------------------------------------------
-- Other
----------------------------------------------------------------------------

encodeBA :: ByteArrayAccess x => x -> Encoding
encodeBA = encodeBytes . convert

decodeBA :: FromByteArray x => Decoder s x
decodeBA = either fail pure . fromByteArray =<< decodeBytes

-- | Type alias for denoting raw bytes. Indended to be used with hashes
-- and signatures, like in type `Hash Raw`, and not type-safe hashing and
-- signing.
-- TODO: probably it makes sense to make it a newtype, like in Cardano?
-- Also, is it the most appropriate place for it?
type Raw = LBS.ByteString
