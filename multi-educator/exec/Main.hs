-- | Starting point for running an Educator node

module Main where

import Options.Applicative (execParser, fullDesc, helper, info, progDesc)

import Dscp.CommonCLI (versionOption)
import Dscp.Config (buildConfig, configParamsParser, rcast)
import Dscp.MultiEducator

main :: IO ()
main = do
    eConfig <- getMultiEducatorConfig
    let wConfig = rcast eConfig
    launchMultiEducatorRealMode eConfig $
        withWitnessConfig wConfig multiEducatorEntry

getMultiEducatorConfig :: IO MultiEducatorConfigRec
getMultiEducatorConfig = do
    let parser = (,) <$> configParamsParser <*> multiEducatorConfigParser
    (configParams, cliConfigMod) <- execParser $
        info (helper <*> versionOption <*> parser) $
        fullDesc <> progDesc "Disciplina multi educator node."
    let wrapConfig cfg = cliConfigMod $ defaultMultiEducatorConfig <> cfg
    buildConfig configParams $
        fmap wrapConfig . fillMultiEducatorConfig
