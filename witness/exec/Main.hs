-- | Starting point for running a Witness node

module Main where

import Options.Applicative (execParser, fullDesc, helper, info, progDesc)

import Dscp.CommonCLI (versionOption)
import Dscp.Config
import Dscp.Witness


main :: IO ()
main = do
    (witnessParams, wConfig) <- getWitnessParams
    launchWitnessRealMode wConfig witnessParams witnessEntry

getWitnessParams :: IO (WitnessParams, WitnessConfigRec)
getWitnessParams = do
    let parser = (,) <$> witnessParamsParser <*> configParamsParser
    (params, configParams) <- execParser $
        info (helper <*> versionOption <*> parser) $
        fullDesc <> progDesc "Disciplina witness node."
    config <- buildConfig configParams fillWitnessConfig
    return (params, config)
