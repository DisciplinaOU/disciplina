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
    { epDbPath       :: !FilePath
    , epLogParams    :: !LoggingParams
    , epSqliteDbPath :: !FilePath
    }

educatorParamsParser :: Parser EducatorParams
educatorParamsParser = do
    epDbPath <- dbPathParser
    epLogParams <- logParamsParser "educator"
    epSqliteDbPath <- sqliteDbPathParser
    return EducatorParams{..}

getEducatorParams :: IO EducatorParams
getEducatorParams =
    execParser $ info (helper <*> versionOption <*> educatorParamsParser) $
    fullDesc <> progDesc "Discplina educator node."
