
-- | Command-line options and flags for Educator node

module EducatorParams
       ( EducatorParams (..)
       , getEducatorParams
       ) where

import Universum

import Options.Applicative (Parser, execParser, fullDesc, helper, info, progDesc)

import Dscp.CLI (dbPathParser, logParamsParser, versionOption)
import Dscp.Launcher (LoggingParams)

data EducatorParams = EducatorParams
    { epDbPath    :: !FilePath
    , epLogParams :: !LoggingParams
    }

educatorParamsParser :: Parser EducatorParams
educatorParamsParser =
    EducatorParams <$> dbPathParser <*> logParamsParser "educator"

getEducatorParams :: IO EducatorParams
getEducatorParams =
    execParser $ info (helper <*> versionOption <*> educatorParamsParser) $
    fullDesc <> progDesc "Discplina educator node."
