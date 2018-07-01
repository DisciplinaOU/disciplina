-- | Aeson instances for all types in Dscp.Crypto.

module Dscp.Crypto.Aeson () where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))

import Dscp.Crypto.Encrypt (Encrypted (..))
import Dscp.Crypto.Instances ()
import Dscp.Util.Aeson (AsByteString (..), Base64)

instance ToJSON a => ToJSON (Encrypted a) where
    toJSON Encrypted{..} = object
        [ "encrypted" .= eCiphertext
        , "auth_tag" .= AsByteString @Base64 eAuthTag
        ]
instance FromJSON a => FromJSON (Encrypted a) where
    parseJSON = withObject "Encrypted item" $ \o -> do
        eCiphertext <- o .: "encrypted"
        (getAsByteString @Base64 -> eAuthTag) <- o .: "auth_tag"
        return Encrypted{..}
