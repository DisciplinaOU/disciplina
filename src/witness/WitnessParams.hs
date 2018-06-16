
-- | Command-line options and flags for Witness nodes

module WitnessParams
       ( WitnessParams (..)
       , getWitnessParams
       ) where

import Universum

import Options.Applicative (Parser, execParser, fullDesc, help, helper, info, long, progDesc)

import Dscp.CLI (dbPathParser, logParamsParser, versionOption)
import Dscp.Launcher (LoggingParams)

data WitnessParams = WitnessParams
    { wpDbPath    :: !FilePath
    , wpLogParams :: !LoggingParams
    }

witnessParamsParser :: Parser WitnessParams
witnessParamsParser =
    WitnessParams <$> dbPathParser <*> logParamsParser "witness"

getWitnessParams :: IO WitnessParams
getWitnessParams =
    execParser $ info (helper <*> versionOption <*> witnessParamsParser) $
    fullDesc <> progDesc "Disciplina witness node."
