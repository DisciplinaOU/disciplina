
-- | Command-line options and flags for Witness nodes

module WitnessParams
       ( WitnessOptParams (..)
       , getWitnessParams
       ) where

import Universum

import Options.Applicative (Parser, execParser, fullDesc, help, helper, info, long, progDesc)

import Disciplina.CLI (dbPathParser, logParamsParser, versionOption)
import Disciplina.Launcher (LoggingParams)

data WitnessOptParams = WitnessOptParams
    { wpDbPath    :: !FilePath
    , wpLogParams :: !LoggingParams
    }

witnessParamsParser :: Parser WitnessOptParams
witnessParamsParser =
    WitnessOptParams <$> dbPathParser <*> logParamsParser "witness"

getWitnessParams :: IO WitnessOptParams
getWitnessParams =
    execParser $ info (helper <*> versionOption <*> witnessParamsParser) $
    fullDesc <> progDesc "Disciplina witness node."
