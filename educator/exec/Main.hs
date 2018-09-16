-- | Starting point for running an Educator node

module Main where

import Options.Applicative (execParser, fullDesc, helper, info, progDesc)

import Dscp.CommonCLI (versionOption)
import Dscp.Config (buildConfig, configParamsParser, rcast)
import Dscp.Educator

main :: IO ()
main = do
    eConfig <- getEducatorConfig
    let wConfig = rcast eConfig
    launchEducatorRealMode eConfig $
        withWitnessConfig wConfig educatorEntry

getEducatorConfig :: IO EducatorConfigRec
getEducatorConfig = do
    configParams <- execParser $
        info (helper <*> versionOption <*> configParamsParser) $
        fullDesc <> progDesc "Disciplina educator node."
    buildConfig configParams fillEducatorConfig
