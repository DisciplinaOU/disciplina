-- | Starting point for running an Educator node

module Main where

import Options.Applicative (execParser, fullDesc, helper, info, progDesc)

import Dscp.CommonCLI (versionOption)
import Dscp.Config (buildConfig, configParamsParser, rcast)
import Dscp.MultiEducator

main :: IO ()
main = do
    eConfig <- getEducatorConfig
    let wConfig = rcast eConfig
    launchEducatorRealMode eConfig $
        withWitnessConfig wConfig educatorEntry

getEducatorConfig :: IO MultiEducatorConfigRec
getEducatorConfig = do
    let parser = (,) <$> configParamsParser <*> multiEducatorConfigParser
    (configParams, cliConfig) <- execParser $
        info (helper <*> versionOption <*> parser) $
        fullDesc <> progDesc "Disciplina educator node."
    buildConfig configParams $
        fmap (<> cliConfig) . fillMultiEducatorConfig
