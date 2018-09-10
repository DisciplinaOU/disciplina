
-- | Web-related types

module Dscp.Web.Types
       ( NetworkAddress (..)
       , parseNetAddr
       ) where

import Fmt (Buildable (..), (+|), (|+))
import Text.Parsec (eof, many1, parse, sepBy)
import Text.Parsec.Char (char, digit)
import qualified Text.Parsec.String as Parsec
import qualified Text.Show

data NetworkAddress = NetworkAddress
    { naHost :: !Text
    , naPort :: !Word16
    } deriving (Eq, Ord, Generic)

instance Buildable NetworkAddress where
    build NetworkAddress {..} = ""+|naHost|+":"+|naPort|+""

instance Show NetworkAddress where
    show = toString . pretty

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
