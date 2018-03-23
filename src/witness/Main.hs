
-- | Starting point for running a Witness node

module Main where

import Universum

import Mockable (runProduction)
import System.Wlog (logInfo, logWarning)

import Disciplina.DB (DBType (WitnessDB))
import Disciplina.Launcher (BasicNodeParams (..), LoggingParams (..), bracketBasicNodeResources,
                            runBasicRealMode)
import Params (WitnessParams (..), getWitnessParams)

main :: IO ()
main = do
    WitnessParams {..} <- getWitnessParams
    let loggingParams = LoggingParams
            { lpDefaultName = "witness"
            , lpDirectory   = wpLogDir
            , lpConfigPath  = wpLogConfig
            }
        basicParams = BasicNodeParams
            { bnpLoggingParams = loggingParams
            , bnpDBType        = WitnessDB
            , bnpDBPath        = wpDbPath
            }
    runProduction . bracketBasicNodeResources basicParams $
        \nr -> runBasicRealMode nr $ do
            logInfo "Hey, here log-warper works!"
            logWarning "Don't forget to implement everything else though!"
