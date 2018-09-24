{-# LANGUAGE DeriveFunctor #-}

-- | Aeson instances for common types and some useful abstractions.

module Dscp.Util.Aeson
    ( AsByteString (..)
    , CustomEncoding (..)
    , Base64Encoded
    , HexEncoded
    , IsEncoding

    , Versioned (..)

    , toJSONSerialise
    , parseJSONSerialise
    ) where

import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, withText, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.ByteArray (ByteArray, ByteArrayAccess)
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Lazy as LBS
import Data.Reflection (Reifies (..))
import qualified Data.SemVer as SemVer
import Fmt ((+||), (||+))
import Servant.Client.Core (BaseUrl, parseBaseUrl, showBaseUrl)
import Time (KnownRatName, Time, unitsF, unitsP)

import qualified Dscp.Crypto.ByteArray as BA
import Dscp.Util (Base (..), fromBase, leftToFail, nothingToFail, toBase)
import Dscp.Util.Test

-- | Often one wants to convert bytestring to JSON, but such convertion
-- is encoding-dependent so we have no corresponding instance because it would
-- be ambiguous.
newtype AsByteString encoding a = AsByteString { getAsByteString :: a }
    deriving (Eq, Ord, Show, Monoid, ByteArrayAccess, ByteArray, Functor)

-- | This one for the case when need custom JSON instances.
newtype CustomEncoding encoding a =
    CustomEncoding { unCustomEncoding :: a }
    deriving (Eq, Ord, Show, Monoid, ByteArrayAccess, ByteArray, Functor)

data Base64Encoded
data HexEncoded

instance Reifies Base64Encoded Base where
    reflect _ = Base64
instance Reifies HexEncoded Base where
    reflect _ = Base16

type IsEncoding enc = Reifies enc Base

instance (BA.ByteArrayAccess a, IsEncoding enc) =>
         ToJSON (AsByteString enc a) where
    toJSON =
        let base = reflect (Proxy @enc)
        in String . toBase base . getAsByteString

instance (BA.FromByteArray a, IsEncoding enc) =>
         FromJSON (AsByteString enc a) where
    parseJSON = withText "encoded text" $ \t ->
        let base = reflect (Proxy @enc)
        in fmap AsByteString . leftToFail $ fromBase base t

-- | Attaches version of JSON serialisation format.
newtype Versioned a = Versioned a
    deriving (Eq, Show)

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

deriving instance Arbitrary a => Arbitrary (AsByteString enc a)
deriving instance Arbitrary a => Arbitrary (CustomEncoding enc a)

instance Arbitrary a => Arbitrary (Versioned a) where
    arbitrary = Versioned <$> arbitrary

toJSONSerialise :: Serialise a => Base -> a -> Value
toJSONSerialise base =
    toJSON . toBase base . LBS.toStrict . serialise

parseJSONSerialise :: Serialise a => Base -> Value -> Parser a
parseJSONSerialise base v =
    parseJSON v
    >>= leftToFail . fromBase base
    >>= leftToFail . first (show @Text) . deserialiseOrFail . LBS.fromStrict

---------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------

instance ToJSON SemVer.Version where
    toJSON = String . SemVer.toText
instance FromJSON SemVer.Version where
    parseJSON = withText "version" $ leftToFail . SemVer.fromText

instance KnownRatName unit => ToJSON (Time unit) where
    toJSON = String . toText . unitsF
instance KnownRatName unit => FromJSON (Time unit) where
    parseJSON = withText "time duration" $
        nothingToFail "Invalid time format" . unitsP . toString

-- TODO: `servant-client-core` dependency in `disciplina-core` is only because
-- of these instances. They are here because they are used simultaneously in
-- `faucet` and `txperf`. Need to move elsewhere
instance FromJSON BaseUrl where
    parseJSON = withText "url" $
        nothingToFail "Invalid URL" . parseBaseUrl . toString
instance ToJSON BaseUrl where
    toJSON = String . toText . showBaseUrl
