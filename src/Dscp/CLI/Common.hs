{-# LANGUAGE ApplicativeDo #-}

-- | Common CLI params

module Disciplina.CLI.Common
       ( logParamsParser
       , dbPathParser
       , versionOption
       ) where

import Universum

import Data.Version (showVersion)
import Options.Applicative (Parser, help, infoOption, long, metavar, optional, strOption, value)
import System.Wlog (LoggerName)

import Disciplina.Launcher.Params (LoggingParams (..))
import Paths_disciplina (version)

logParamsParser :: LoggerName -> Parser LoggingParams
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

versionOption :: Parser (a -> a)
versionOption = infoOption ("disciplina-" <> (showVersion version)) $
    long "version" <>
    help "Show version."
