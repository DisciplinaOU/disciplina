-- | Fetching secret from input.

module KeygenSecret
       ( SecretDataType (..)
       , parseInputWithSecret
       ) where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as LBS

import Dscp.Core.Governance
import Dscp.Crypto
import Dscp.Resource.Keys
import Dscp.Util
import Dscp.Util.Serialise

-- | All ways to convert given text to raw data we want be able to use when
-- parsing CLI options.
bytestringDecoders :: FromByteArray a => [ByteString -> Either String a]
bytestringDecoders = [fromByteArray, fromBase64 . decodeUtf8, fromHex . decodeUtf8]

-- | Tries to interpret given bytestring as raw data or hex/base64.
parseBytesWith :: Show a => Show b => FromByteArray a => (a -> Maybe b) -> ByteString -> Maybe b
parseBytesWith interpret =
    asum . map ((interpret =<<) . eitherToMaybe) . sequence bytestringDecoders

-- | How to interpret given input into a secret key.
data SecretDataType
    = PlainSecret (Maybe PassPhrase)
    | KeyfileSecret (Maybe PassPhrase)
    | CommSecret Integer
    | SecretFromSeed
    deriving (Show)

parseInputWithSecret :: SecretDataType -> ByteString -> Maybe SecretKey
parseInputWithSecret = \case
    PlainSecret mpass -> \input -> asum
        [ do
            parseBytesWith pure $ input
        , do
            let pp = mpass ?: emptyPassPhrase
            encSecret <- parseBytesWith (eitherToMaybe . deserialiseOrFail') input
            leftToFail @Text . first show $ decrypt pp encSecret
        ]
    KeyfileSecret mpass -> \input -> do
        let pp = mpass ?: emptyPassPhrase
        content <- decode $ LBS.fromStrict input
        fromKeyfileContent pp content
    CommSecret n -> \input ->
         flip committeeDerive n <$> eitherToMaybe (mkCommitteeSecret input)
    SecretFromSeed -> secretFromSeed
