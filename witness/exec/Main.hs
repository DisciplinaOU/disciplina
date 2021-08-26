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
    let parser = (,) <$> configParamsParser <*> witnessConfigParser
    (configParams, cliConfigMod) <- execParser $
        info (helper <*> versionOption <*> parser) $
        fullDesc <> progDesc "Disciplina witness node."
    let wrapConfig cfg = cliConfigMod $ defaultWitnessConfig <> cfg
    buildConfig configParams $
        fmap wrapConfig . fillWitnessConfig
