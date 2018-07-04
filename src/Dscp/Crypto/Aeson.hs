-- | Aeson instances for all types in Dscp.Crypto.

module Dscp.Crypto.Aeson () where

import Prelude hiding (toStrict)

import Data.Aeson (FromJSON (..), ToJSON (..))

import Dscp.Crypto.ByteArray (FromByteArray)
import Dscp.Crypto.Encrypt (Encrypted)
import Dscp.Crypto.Instances ()
import Dscp.Util.Aeson (toJSONSerialise, parseJSONSerialise)
import Dscp.Util (Base(Base64))

instance FromByteArray a => ToJSON (Encrypted a) where
    toJSON = toJSONSerialise Base64
instance FromByteArray a => FromJSON (Encrypted a) where
    parseJSON = parseJSONSerialise Base64
