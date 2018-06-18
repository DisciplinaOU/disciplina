{-# LANGUAGE ApplicativeDo #-}

-- | Common CLI params

module Dscp.CLI.Common
       ( logParamsParser
       , dbPathParser
       , sqliteDbPathParser
       , versionOption
       ) where

import Universum

import Data.Version (showVersion)
import qualified Loot.Log as Log
import Options.Applicative (Parser, help, infoOption, long, metavar, optional, strOption, value)

import Dscp.Launcher.Params (LoggingParams (..))
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

dbPathParser :: Parser FilePath
dbPathParser = strOption $
    long "db-path" <>
    metavar "FILEPATH" <>
    help "Path to database directory for witness node." <>
    value "witness-db"

sqliteDbPathParser :: Parser FilePath
sqliteDbPathParser = strOption $
    long "db-path" <>
    metavar "FILEPATH" <>
    help "Path to database directory for educator's private data." <>
    value "educator-db"

versionOption :: Parser (a -> a)
versionOption = infoOption ("disciplina-" <> (showVersion version)) $
    long "version" <>
    help "Show version."
