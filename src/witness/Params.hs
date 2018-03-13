{-# LANGUAGE ApplicativeDo #-}

-- | Command-line options and flags for Witness nodes

module Params
       ( WitnessParams (..)
       , getWitnessParams
       ) where

import Universum

import Data.Version (showVersion)
import Options.Applicative (Parser, execParser, fullDesc, help, helper, info, infoOption, long,
                            metavar, optional, progDesc, strOption, value)

import Paths_disciplina (version)

data WitnessParams = WitnessParams
    { wpDbPath    :: !FilePath
    , wpLogConfig :: !(Maybe FilePath)
    , wpLogDir    :: !(Maybe FilePath)
    }

witnessParamsParser :: Parser WitnessParams
witnessParamsParser = do
    wpDbPath <- dbPathParser
    wpLogConfig <- logConfigParser
    wpLogDir <- logDirParser
    return WitnessParams {..}
  where
    dbPathParser :: Parser FilePath
    dbPathParser = strOption $
        long "db-path" <>
        metavar "FILEPATH" <>
        help "Path to database directory for witness node." <>
        value "witness-db"
    logConfigParser = optional $ strOption $
        long "log-config" <>
        metavar "FILEPATH" <>
        help "Path to logger configuration."
    logDirParser = optional $ strOption $
        long "log-dir" <>
        metavar "FILEPATH" <>
        help "Path to logs directory."

getWitnessParams :: IO WitnessParams
getWitnessParams =
    execParser $ info (helper <*> versionOption <*> witnessParamsParser) $
    fullDesc <> progDesc "Disciplina witness node."
  where
    versionOption = infoOption ("disciplina-" <> (showVersion version)) $
        long "version" <>
        help "Show version."
