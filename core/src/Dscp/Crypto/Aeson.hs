{-# LANGUAGE TypeOperators #-}

-- | Aeson instances for all types in Dscp.Crypto.

module Dscp.Crypto.Aeson () where

import Prelude hiding (toStrict)

import Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..), ToJSON (..),
                   ToJSONKey (..), object, withObject, (.:), (.=))
import Data.Aeson.Types (toJSONKeyText)
import Data.ByteArray (ByteArrayAccess, convert)

import Dscp.Crypto.ByteArray
import Dscp.Crypto.Encrypt
import Dscp.Crypto.Hash
import Dscp.Crypto.MerkleTree
import Dscp.Crypto.Signing
import Dscp.Util
import Dscp.Util.Aeson

---------------------------------------------------------------------------
-- Encrypted data and passphrases
---------------------------------------------------------------------------

instance ToJSON PassPhrase where
    toJSON = toJSON . decodeUtf8 @Text @ByteString . convert
instance FromJSON PassPhrase where
    parseJSON = fmap (convert . encodeUtf8 @Text @ByteString) . parseJSON

---------------------------------------------------------------------------
-- Hashes
---------------------------------------------------------------------------

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

---------------------------------------------------------------------------
-- Keys and signatures
---------------------------------------------------------------------------

instance ByteArrayAccess (AbstractPK ss) => ToJSON (AbstractPK ss) where
    toJSON = toJSON . AsByteString @HexEncoded
instance FromByteArray (AbstractPK ss) => FromJSON (AbstractPK ss) where
    parseJSON = fmap (getAsByteString @HexEncoded) . parseJSON

instance ByteArrayAccess (AbstractSig ss a) => ToJSON (AbstractSig ss a) where
    toJSON = toJSON . AsByteString @HexEncoded
instance FromByteArray (AbstractSig ss a) => FromJSON (AbstractSig ss a) where
    parseJSON = fmap (getAsByteString @HexEncoded) . parseJSON

---------------------------------------------------------------------------
-- Merkle trees
---------------------------------------------------------------------------

instance ToJSON (MerkleSignature a) where
    toJSON MerkleSignature{..} = object
        [ "root" .= msHash
        , "transactionsNum" .= msSize
        ]

instance FromJSON (MerkleSignature a) where
    parseJSON = withObject "merkle signature" $ \o -> do
        msHash <- o .: "root"
        msSize <- o .: "transactionsNum"
        return MerkleSignature{..}
