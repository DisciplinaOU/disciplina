-- | Starting point for running an Educator node

module Main where

import Loot.Log (logInfo, logWarning, modifyLogName)
import Options.Applicative (execParser, fullDesc, helper, info, progDesc)

import Dscp.CommonCLI (versionOption)
import Dscp.Config (buildConfig, configParamsParser)
import Dscp.Educator
import Dscp.Educator.Web.Server

main :: IO ()
main = do
    (educatorParams, educatorConfig) <- getEducatorParams
    launchEducatorRealMode educatorConfig educatorParams $
      modifyLogName (<> "node") $ do
        logInfo "This is the stub for Educator node executable"
        logWarning "Please don't forget to implement everything else!"

        serveStudentAPIReal (epWebParams educatorParams)

getEducatorParams :: IO (EducatorParams, EducatorConfigRec)
getEducatorParams = do
    let parser = (,) <$> educatorParamsParser <*> configParamsParser
    (params, configPath) <- execParser $
        info (helper <*> versionOption <*> parser) $
        fullDesc <> progDesc "Disciplina educator node."
    config <- buildConfig configPath fillEducatorConfig
    return (params, config)
