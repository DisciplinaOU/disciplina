
-- | Web-related types

module Dscp.Web.Types
       ( NetworkAddress (..)
       , parseNetAddr
       , AsClientT
       ) where

import Fmt (Buildable (..), (+|), (|+))
import Servant.Client (Client)
import Servant.Generic ((:-))
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

---------------------------------------------------------------------------
-- Servant
---------------------------------------------------------------------------

-- todo: isn't needed with servant-client-0.14 (lts-12)
data AsClientT (m :: * -> *)
type instance AsClientT m :- api = Client m api
