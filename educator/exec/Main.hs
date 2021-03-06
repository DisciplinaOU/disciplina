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
    let parser = (,) <$> configParamsParser <*> educatorConfigParser
    (configParams, cliConfigMod) <- execParser $
        info (helper <*> versionOption <*> parser) $
        fullDesc <> progDesc "Disciplina educator node."
    let wrapConfig cfg = cliConfigMod $ defaultEducatorConfig <> cfg
    buildConfig configParams $
        fmap wrapConfig . fillEducatorConfig
