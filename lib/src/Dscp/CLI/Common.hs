{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes   #-}

-- | Common CLI params

module Dscp.CLI.Common
       ( logParamsParser
       , rocksParamsParser
       , sqliteParamsParser
       , versionOption
       , keyParamsParser

       , peersParser
       , ourZTNodeIdParser
       , netCliParamsParser
       , netServParamsParser

       , networkAddressParser
       ) where

import qualified Data.Set as Set
import Data.Version (showVersion)
import qualified Loot.Log as Log
import Loot.Network.ZMQ.Common (ZTNodeId (..), parseZTNodeId)
import Options.Applicative (Parser, eitherReader, help, infoOption, long, metavar, option, optional,
                            str, strOption, switch, value)
import Text.InterpolatedString.Perl6 (qc)
import Text.Parsec (eof, many1, parse, sepBy)
import Text.Parsec.Char (char, digit)
import qualified Text.Parsec.String as Parsec

import Dscp.Crypto (mkPassPhrase)
import Dscp.DB.Rocks.Real.Types (RocksDBParams (..))
import Dscp.DB.SQLite.Types (SQLiteDBLocation (..), SQLiteParams (..))
import Dscp.Resource.Keys (KeyParams (..))
import Dscp.Resource.Logging (LoggingParams (..))
import Dscp.Resource.Network (NetCliParams (..), NetServParams (..))
import Dscp.Util (leftToFail)
import Dscp.Web (NetworkAddress (..))
import Paths_disciplina (version)

logParamsParser :: Log.Name -> Parser LoggingParams
logParamsParser lpDefaultName = do
    lpDebug <- logDebugParser
    lpConfigPath <- logConfigParser
    lpDirectory <- logDirParser
    return LoggingParams {..}
  where
    logDebugParser = switch $
        long "debug" <>
        help "Switch default logging level from Info to Debug"
    logConfigParser = optional $ strOption $
        long "log-config" <>
        metavar "FILEPATH" <>
        help "Path to logger configuration."
    logDirParser = optional $ strOption $
        long "log-dir" <>
        metavar "FILEPATH" <>
        help "Path to logs directory."

rocksParamsParser :: Parser RocksDBParams
rocksParamsParser = fmap RocksDBParams $ strOption $
    long "db-path" <>
    metavar "FILEPATH" <>
    help "Path to database directory for witness node." <>
    value "witness-db"

sqliteParamsParser :: Parser SQLiteParams
sqliteParamsParser = fmap (SQLiteParams . SQLiteReal) $ strOption $
    long "sql-path" <>
    metavar "FILEPATH" <>
    help "Path to database directory for educator's private data." <>
    value "educator-db"

versionOption :: Parser (a -> a)
versionOption = infoOption ("disciplina-" <> (showVersion version)) $
    long "version" <>
    help "Show version."

keyParamsParser :: Text -> Parser KeyParams
keyParamsParser who = do
    kpPath <- kpKeyPathParser
    kpGenNew <- kpGenKeyParser
    kpPassphrase <- kpPassphraseParser
    pure KeyParams{..}
  where
    kpKeyPathParser = optional . strOption $
         long [qc|{who}-keyfile-path|] <>
         metavar "FILEPATH" <>
         help [qc|Path to the secret key of {who}.|]
    kpGenKeyParser = switch $
         long [qc|{who}-generate-new-key|] <>
         help [qc|Generate the key and write it to '{who}-keyfile-path' path.|]
    kpPassphraseParser = optional . option passphraseReadM $
         long [qc|{who}-keyfile-password|] <>
         metavar "PASSWORD" <>
         help "Password of secret key."
    passphraseReadM = leftToFail . first pretty . mkPassPhrase =<< str


----------------------------------------------------------------------------
-- ZMQ TCP
----------------------------------------------------------------------------

-- | Parse peers to connect to.
peersParser :: Parser (Set ZTNodeId)
peersParser =
    fmap Set.fromList $
    many $
    option (eitherReader parseZTNodeId)
           (long "peer" <> metavar "HOST:PORT1:PORT2" <> help "Peer(s) we should connect to")

-- | Parser for ZTNodeId we will bind on.
ourZTNodeIdParser :: Parser ZTNodeId
ourZTNodeIdParser = do
    option (eitherReader parseZTNodeId)
           (long "bind" <> metavar "HOST:PORT1:PORT2" <> help "Host/ports to bind on")

netCliParamsParser :: Parser NetCliParams
netCliParamsParser = NetCliParams <$> peersParser

netServParamsParser :: Parser NetServParams
netServParamsParser = NetServParams <$> peersParser <*> ourZTNodeIdParser

---------------------------------------------------------------------------
-- Utils
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

networkAddressParser :: String -> String -> Parser NetworkAddress
networkAddressParser pName helpTxt =
    option (eitherReader parseNetAddr) $
    long pName <>
    metavar "HOST:PORT" <>
    help helpTxt
