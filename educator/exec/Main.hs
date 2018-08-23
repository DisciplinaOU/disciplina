-- | Starting point for running an Educator node

module Main where

import Options.Applicative (execParser, fullDesc, helper, info, progDesc)

import Dscp.CommonCLI (versionOption)
import Dscp.Config (buildConfig, configParamsParser)
import Dscp.Educator

main :: IO ()
main = do
    (educatorParams, educatorConfig) <- getEducatorParams
    launchEducatorRealMode educatorConfig educatorParams educatorEntry

getEducatorParams :: IO (EducatorParams, EducatorConfigRec)
getEducatorParams = do
    let parser = (,) <$> educatorParamsParser <*> configParamsParser
    (params, configPath) <- execParser $
        info (helper <*> versionOption <*> parser) $
        fullDesc <> progDesc "Disciplina educator node."
    config <- buildConfig configPath fillEducatorConfig
    return (params, config)
