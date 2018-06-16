
-- | Command-line options and flags for Educator node

module EducatorParams
       ( EducatorOptParams (..)
       , getEducatorParams
       ) where

import Universum

import Options.Applicative (Parser, execParser, fullDesc, helper, info, progDesc)

import Disciplina.CLI (dbPathParser, logParamsParser, versionOption)
import Disciplina.Launcher (LoggingParams)

data EducatorOptParams = EducatorOptParams
    { epDbPath    :: !FilePath
    , epLogParams :: !LoggingParams
    }

educatorParamsParser :: Parser EducatorOptParams
educatorParamsParser =
    EducatorOptParams <$> dbPathParser <*> logParamsParser "educator"

getEducatorParams :: IO EducatorOptParams
getEducatorParams =
    execParser $ info (helper <*> versionOption <*> educatorParamsParser) $
    fullDesc <> progDesc "Discplina educator node."
