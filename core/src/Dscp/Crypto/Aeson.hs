{-# LANGUAGE TypeOperators #-}

-- | Aeson instances for all types in Dscp.Crypto.

module Dscp.Crypto.Aeson () where

import Prelude hiding (toStrict)

import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.ByteArray (ByteArrayAccess)
import Data.Reflection (Reifies (..))

import Dscp.Crypto.ByteArray (FromByteArray)
import Dscp.Crypto.Encrypt (Encrypted)
import Dscp.Crypto.Hash (AbstractHash)
import Dscp.Crypto.MerkleTree
import Dscp.Crypto.Signing (AbstractPK, AbstractSig)
import Dscp.Util.Aeson (AsByteString (..), CustomEncoding (..), HexEncoded, IsEncoding,
                        parseJSONSerialise, toJSONSerialise)

instance (FromByteArray a, IsEncoding enc) =>
         ToJSON (CustomEncoding enc $ Encrypted a) where
    toJSON = toJSONSerialise (reflect (Proxy @enc)) . unCustomEncoding
instance (FromByteArray a, IsEncoding enc) =>
         FromJSON (CustomEncoding enc $ Encrypted a) where
    parseJSON = fmap CustomEncoding . parseJSONSerialise (reflect (Proxy @enc))

instance ByteArrayAccess (AbstractHash hf a) => ToJSON (AbstractHash hf a) where
    toJSON = toJSON . AsByteString @HexEncoded
instance FromByteArray (AbstractHash hf a) => FromJSON (AbstractHash hf a) where
    parseJSON = fmap (getAsByteString @HexEncoded) . parseJSON

instance ByteArrayAccess (AbstractPK ss) => ToJSON (AbstractPK ss) where
    toJSON = toJSON . AsByteString @HexEncoded
instance FromByteArray (AbstractPK ss) => FromJSON (AbstractPK ss) where
    parseJSON = fmap (getAsByteString @HexEncoded) . parseJSON

instance ByteArrayAccess (AbstractSig ss a) => ToJSON (AbstractSig ss a) where
    toJSON = toJSON . AsByteString @HexEncoded
instance FromByteArray (AbstractSig ss a) => FromJSON (AbstractSig ss a) where
    parseJSON = fmap (getAsByteString @HexEncoded) . parseJSON

instance (IsEncoding enc) =>
         ToJSON (CustomEncoding enc $ MerkleSignature a) where
    toJSON = toJSONSerialise (reflect (Proxy @enc)). unCustomEncoding
instance (IsEncoding enc) =>
         FromJSON (CustomEncoding enc $ MerkleSignature a) where
    parseJSON = fmap CustomEncoding . parseJSONSerialise (reflect (Proxy @enc))

instance (Serialise a, IsEncoding enc) =>
         ToJSON (CustomEncoding enc $ MerkleProof a) where
    toJSON = toJSONSerialise (reflect (Proxy @enc)). unCustomEncoding
instance (Serialise a, IsEncoding enc) =>
         FromJSON (CustomEncoding enc $ MerkleProof a) where
    parseJSON = fmap CustomEncoding . parseJSONSerialise (reflect (Proxy @enc))
