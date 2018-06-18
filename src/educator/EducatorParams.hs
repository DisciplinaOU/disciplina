{-# LANGUAGE ApplicativeDo #-}

-- | Command-line options and flags for Educator node

module EducatorParams
       ( EducatorParams (..)
       , getEducatorParams
       ) where

import Universum

import Options.Applicative (Parser, execParser, fullDesc, helper, info, progDesc)

import Dscp.CLI (dbPathParser, logParamsParser, sqliteDbPathParser, versionOption)
import Dscp.Launcher (LoggingParams)

data EducatorParams = EducatorParams
    { epRocksDbPath  :: !FilePath
    , epLogParams    :: !LoggingParams
    , epSqliteDbPath :: !FilePath
    }

educatorParamsParser :: Parser EducatorParams
educatorParamsParser = do
    epRocksDbPath <- dbPathParser
    epLogParams <- logParamsParser "educator"
    epSqliteDbPath <- sqliteDbPathParser
    return EducatorParams{..}

getEducatorParams :: IO EducatorParams
getEducatorParams =
    execParser $ info (helper <*> versionOption <*> educatorParamsParser) $
    fullDesc <> progDesc "Discplina educator node."
