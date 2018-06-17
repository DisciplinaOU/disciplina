{-# LANGUAGE ApplicativeDo #-}

-- | Common CLI params

module Dscp.CLI.Common
       ( logParamsParser
       , rocksParamsParser
       , sqliteParamsParser
       , versionOption

       , peersParser
       , ourZTNodeIdParser
       , netCliParamsParser
       , netServParamsParser
       ) where

import Universum

import qualified Data.Set as Set
import Data.Version (showVersion)
import qualified Loot.Log as Log
import Loot.Network.ZMQ.Common (ZTNodeId (..), parseZTNodeId)
import Options.Applicative (Parser, eitherReader, help, infoOption, long, metavar, option, optional,
                            strOption, value)
import System.Wlog (LoggerName)

import Dscp.DB.Rocks.Real.Types (RocksDBParams (..))
import Dscp.DB.SQLite.Types (SQLiteDBLocation (..), SQLiteParams (..))
import Dscp.Resource.Logging (LoggingParams (..))
import Dscp.Resource.Network (NetCliParams (..), NetServParams (..))
import Paths_disciplina (version)

logParamsParser :: Log.Name -> Parser LoggingParams
logParamsParser lpDefaultName = do
    lpConfigPath <- logConfigParser
    lpDirectory <- logDirParser
    return LoggingParams {..}
  where
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
