-- | Aeson instances for common types and some useful abstractions.

module Dscp.Util.Aeson
    ( AsByteString (..)
    , Base64

    , Versioned (..)
    ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, withText, (.:), (.=))
import qualified Data.ByteArray as BA
import qualified Data.SemVer as SemVer
import Fmt ((+||), (||+))
import qualified Serokell.Util.Base64 as Base64

import qualified Dscp.Crypto.ByteArray as BA
import Dscp.Util (leftToFail)

-- | Often one wants to convert bytestring to JSON, but such convertion
-- is encoding-dependent so we have no such instance.
newtype AsByteString encoding a = AsByteString { getAsByteString :: a }

-- Text <-> ByteString conversion formats
data Base64

instance BA.ByteArrayAccess a => ToJSON (AsByteString Base64 a) where
    toJSON a = String . Base64.encode . BA.convert $ getAsByteString a
instance BA.FromByteArray a => FromJSON (AsByteString Base64 a) where
    parseJSON = withText "base64 text" $ \t -> do
        bs <- Base64.decode t
            & leftToFail . first ("Invalid base64 string: " <>)
        res <- BA.fromByteArray bs
            & leftToFail . first ("Malformed object representation: " <>)
        return $ AsByteString res

-- | Attaches version of JSON serialisation format.
newtype Versioned a = Versioned a

-- | We have only one version. Till the moment proper JSON versioning framework
-- is delivered.
theVersion :: SemVer.Version
theVersion = SemVer.version 1 0 0 [] []

instance ToJSON a => ToJSON (Versioned a) where
    toJSON (Versioned content) = object
        [ "version" .= theVersion
        , "content" .= content
        ]

instance FromJSON a => FromJSON (Versioned a) where
    parseJSON = withObject "Versioned content" $ \o -> do
        ver <- o .: "version"
        unless (ver == theVersion) $
            fail $ "Only "+||theVersion||+" version is supported"

        content <- o .: "content"
        return $ Versioned content

---------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------

instance ToJSON SemVer.Version where
    toJSON = String . SemVer.toText
instance FromJSON SemVer.Version where
    parseJSON = withText "version" $ leftToFail . SemVer.fromText
