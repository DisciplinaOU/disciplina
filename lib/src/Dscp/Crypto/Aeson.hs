-- | Aeson instances for all types in Dscp.Crypto.

module Dscp.Crypto.Aeson () where

import Prelude hiding (toStrict)

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.ByteArray (ByteArrayAccess)

import Dscp.Crypto.ByteArray (FromByteArray)
import Dscp.Crypto.Encrypt (Encrypted)
import Dscp.Crypto.Hash (AbstractHash)
import Dscp.Crypto.Signing (AbstractPK, AbstractSig)
import Dscp.Util (Base (Base64))
import Dscp.Util.Aeson (AsByteString (..), Base64Encoded, parseJSONSerialise, toJSONSerialise)

asBase64 :: a -> AsByteString Base64Encoded a
asBase64 = AsByteString

getBase64 :: AsByteString Base64Encoded a -> a
getBase64 = getAsByteString

instance FromByteArray a => ToJSON (Encrypted a) where
    toJSON = toJSONSerialise Base64
instance FromByteArray a => FromJSON (Encrypted a) where
    parseJSON = parseJSONSerialise Base64

instance ByteArrayAccess (AbstractHash hf a) => ToJSON (AbstractHash hf a) where
    toJSON = toJSON . asBase64
instance FromByteArray (AbstractHash hf a) => FromJSON (AbstractHash hf a) where
    parseJSON = fmap getBase64 . parseJSON

instance ByteArrayAccess (AbstractPK ss) => ToJSON (AbstractPK ss) where
    toJSON = toJSON . asBase64
instance FromByteArray (AbstractPK ss) => FromJSON (AbstractPK ss) where
    parseJSON = fmap getBase64 . parseJSON

instance ByteArrayAccess (AbstractSig ss a) => ToJSON (AbstractSig ss a) where
    toJSON = toJSON . asBase64
instance FromByteArray (AbstractSig ss a) => FromJSON (AbstractSig ss a) where
    parseJSON = fmap getBase64 . parseJSON
