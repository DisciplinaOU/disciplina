-- | Starting point for running an Educator node

module Main where

import Loot.Log (logInfo, logWarning, modifyLogName)
import Options.Applicative (execParser, fullDesc, helper, info, progDesc)

import Dscp.CommonCLI (versionOption)
import Dscp.Config (buildBaseConfig, configPathParser)
import Dscp.Educator (EducatorConfig, EducatorParams (..), educatorParamsParser,
                      launchEducatorRealMode, serveStudentAPIReal)

main :: IO ()
main = do
    (educatorParams, educatorConfig) <- getEducatorParams
    launchEducatorRealMode educatorConfig educatorParams $
      modifyLogName (<> "node") $ do
        logInfo "This is the stub for Educator node executable"
        logWarning "Please don't forget to implement everything else!"

        serveStudentAPIReal (epWebParams educatorParams)

getEducatorParams :: IO (EducatorParams, EducatorConfig)
getEducatorParams = do
    let parser = (,) <$> educatorParamsParser <*> configPathParser
    (params, configPath) <- execParser $
        info (helper <*> versionOption <*> parser) $
        fullDesc <> progDesc "Disciplina educator node."
    config <- buildBaseConfig configPath
    return (params, config)
