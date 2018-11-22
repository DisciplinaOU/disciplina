{-# LANGUAGE TypeOperators #-}

-- | Aeson instances for all types in Dscp.Crypto.

module Dscp.Crypto.Aeson () where

import Prelude hiding (toStrict)

import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..), ToJSON (..),
                   ToJSONKey (..), object, withObject, (.:), (.=))
import Data.Aeson.Types (toJSONKeyText)
import Data.ByteArray (ByteArrayAccess, convert)
import Data.Reflection (Reifies (..))

import Dscp.Crypto.ByteArray (FromByteArray)
import Dscp.Crypto.Encrypt (Encrypted, PassPhrase)
import Dscp.Crypto.Hash (AbstractHash)
import Dscp.Crypto.MerkleTree
import Dscp.Crypto.Signing (AbstractPK, AbstractSig)
import Dscp.Util (fromHex, leftToFail, toHex)
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

instance ByteArrayAccess (AbstractHash hf a) =>
         ToJSONKey (AbstractHash hf a) where
    toJSONKey = toJSONKeyText toHex
instance FromByteArray (AbstractHash hf a) =>
         FromJSONKey (AbstractHash hf a) where
    fromJSONKey = FromJSONKeyTextParser $ leftToFail . fromHex

instance ByteArrayAccess (AbstractPK ss) => ToJSON (AbstractPK ss) where
    toJSON = toJSON . AsByteString @HexEncoded
instance FromByteArray (AbstractPK ss) => FromJSON (AbstractPK ss) where
    parseJSON = fmap (getAsByteString @HexEncoded) . parseJSON

instance ByteArrayAccess (AbstractSig ss a) => ToJSON (AbstractSig ss a) where
    toJSON = toJSON . AsByteString @HexEncoded
instance FromByteArray (AbstractSig ss a) => FromJSON (AbstractSig ss a) where
    parseJSON = fmap (getAsByteString @HexEncoded) . parseJSON

instance ToJSON PassPhrase where
    toJSON = toJSON . decodeUtf8 @Text @ByteString . convert
instance FromJSON PassPhrase where
    parseJSON = fmap (convert . encodeUtf8 @Text @ByteString) . parseJSON

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

instance ToJSON (MerkleSignature a) where
    toJSON MerkleSignature{..} = object
        [ "root" .= mrHash
        , "transactionsNum" .= mrSize
        ]

instance FromJSON (MerkleSignature a) where
    parseJSON = withObject "merkle signature" $ \o -> do
        mrHash <- o .: "root"
        mrSize <- o .: "transactionsNum"
        return MerkleSignature{..}
