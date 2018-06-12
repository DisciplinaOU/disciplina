
-- | Command-line options and flags for Educator node

module EducatorParams
       ( EducatorParams (..)
       , getEducatorParams
       ) where

import Universum

import Options.Applicative (Parser, execParser, fullDesc, helper, info, progDesc)

import Disciplina.CLI (dbPathParser, logParamsParser, versionOption)
import Disciplina.Launcher (LoggingParams)

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
