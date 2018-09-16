-- | Starting point for running a Witness node

module Main where

import Options.Applicative (execParser, fullDesc, helper, info, progDesc)

import Dscp.CommonCLI (versionOption)
import Dscp.Config
import Dscp.Witness


main :: IO ()
main = do
    wConfig <- getWitnessConfig
    launchWitnessRealMode wConfig witnessEntry

getWitnessConfig :: IO WitnessConfigRec
getWitnessConfig = do
    configParams <- execParser $
        info (helper <*> versionOption <*> configParamsParser) $
        fullDesc <> progDesc "Disciplina witness node."
    buildConfig configParams fillWitnessConfig
