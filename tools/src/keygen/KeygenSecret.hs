-- | Fetching secret from input.

module KeygenSecret
       ( SecretDataType (..)
       , parseInputWithSecret
       ) where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as LBS

import Dscp.Crypto
import Dscp.Resource.Keys
import Dscp.Util
import Dscp.Util.Serialise

-- | All ways to convert given text to raw data we want be able to use when
-- parsing CLI options.
bytestringDecoders :: [Text -> Either String ByteString]
bytestringDecoders = [fromBase64, fromHex, pure . encodeUtf8]

parseBytes :: ByteString -> Maybe ByteString
parseBytes =
    asum . map eitherToMaybe . sequence bytestringDecoders . decodeUtf8

data SecretDataType
    = PlainSecret (Maybe PassPhrase)
    | KeyfileSecret (Maybe PassPhrase)
    | SecretFromSeed

parseInputWithSecret :: SecretDataType -> ByteString -> Maybe SecretKey
parseInputWithSecret = \case
    PlainSecret mpass -> \input -> asum
        [ do
            bs <- parseBytes input
            leftToFail @Text . first show $ deserialiseOrFail' bs
        , do
            let pp = mpass ?: emptyPassPhrase
            bs <- parseBytes input
            encSecret <- leftToFail @Text . first show $ deserialiseOrFail' bs
            leftToFail @Text . first show $ decrypt pp encSecret
        ]
    KeyfileSecret mpass -> \input -> do
        let pp = mpass ?: emptyPassPhrase
        content <- decode $ LBS.fromStrict input
        fromKeyfileContent pp content
    SecretFromSeed -> \input -> asum
        [ do
            -- we need this clause as soon as many code uses
            -- 'withIntSeed' for generation, for instance list of students
            -- known to educator in Student API
            seed <- readMaybe @Word . toString @Text $ decodeUtf8 input
            return $ withIntSeed (fromIntegral seed) genSecretKey
        , do
            return $ withSeed input genSecretKey
        ]
