-- | Starting point for running an Educator node

module Main where

import Loot.Log (logInfo, logWarning, modifyLogName)
import Options.Applicative (execParser, fullDesc, helper, info, progDesc)

import Dscp.CLI (versionOption)
import Dscp.Educator (EducatorParams (..), educatorParamsParser, launchEducatorRealMode)

main :: IO ()
main = do
    educatorParams <- getEducatorParams
    launchEducatorRealMode educatorParams $
      modifyLogName (<> "node") $ do
        logInfo "This is the stub for Educator node executable"
        logWarning "Please don't forget to implement everything else!"

getEducatorParams :: IO EducatorParams
getEducatorParams =
    execParser $ info (helper <*> versionOption <*> educatorParamsParser) $
    fullDesc <> progDesc "Disciplina educator node."
