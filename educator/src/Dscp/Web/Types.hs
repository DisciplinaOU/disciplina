-- | Web-related types

module Dscp.Web.Types
       ( NetworkAddress (..)
       , parseNetAddr
       , GeneralBackendError (..)
       ) where

import Universum

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Fmt (Buildable (..), (+|), (|+), pretty)
import Servant.Server (err400, err503)
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary)
import Text.Parsec (eof, many1, parse, sepBy)
import Text.Parsec.Char (char, digit)
import qualified Text.Parsec.String as Parsec
import qualified Text.Show

import Dscp.Util.Test (Arbitrary (..))
import Dscp.Web.Class

data NetworkAddress = NetworkAddress
    { naHost :: !Text
    , naPort :: !Word16
    } deriving (Eq, Ord, Generic)

instance Buildable NetworkAddress where
    build NetworkAddress {..} = ""+|naHost|+":"+|naPort|+""

instance Show NetworkAddress where
    show = toString @Text . pretty

instance Arbitrary NetworkAddress where
    arbitrary = NetworkAddress <$> arbitrary <*> arbitrary

---------------------------------------------------------------------------
-- Util functions for working with web types
---------------------------------------------------------------------------

parseNetAddr :: String -> Either String NetworkAddress
parseNetAddr st =
    first niceError $ parse parseNA "" st
  where
    niceError = const "Invalid Network Address"
    parseNA :: Parsec.Parser NetworkAddress
    parseNA = NetworkAddress <$> parseHost <* char ':'
                             <*> parsePort <* eof
    parseHost = do host <- parseByte `sepBy` (char '.')
                   unless (length host == 4) $ fail "invalid"
                   return $ toText $ intercalate "." $ (map show host)
    parsePort = parseWord 16
    parseByte = parseWord 8 :: Parsec.Parser Integer
    parseWord n = do x <- fromMaybe (error "unexpected") . readMaybe <$> many1 digit
                     when ((x :: Integer) > 2 ^ (n :: Integer) - 1) $ fail "invalid"
                     return $ fromIntegral x

---------------------------------------------------------------------------
-- Errors
---------------------------------------------------------------------------

-- | General server errors.
data GeneralBackendError
    = InvalidFormat
      -- ^ Decoding failed.
    | ServiceUnavailable Text
      -- ^ Service is overloaded with requests.
    deriving (Show, Eq, Generic)

deriveJSON defaultOptions ''GeneralBackendError

instance Buildable GeneralBackendError where
    build InvalidFormat =
        "Invalid format of the request"
    build (ServiceUnavailable msg) =
        "Service unavailable: " <> build msg

instance HasErrorTag GeneralBackendError where
    errorTag = \case
        InvalidFormat -> "InvalidFormat"
        ServiceUnavailable{} -> "ServiceUnavailable"

instance ToServantErr GeneralBackendError where
    toServantErrNoBody = \case
        InvalidFormat{} -> err400
        ServiceUnavailable{} -> err503

instance Arbitrary GeneralBackendError where
    arbitrary = genericArbitrary
