
-- | Command-line options and flags for Educator node

module Params
       ( educatorParamsParser
       , getEducatorParams
       ) where

import Universum

import Data.Version (showVersion)
import Options.Applicative (Parser, execParser, fullDesc, help, helper, info, infoOption, long,
                            progDesc)

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
