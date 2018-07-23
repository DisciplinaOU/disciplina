-- | Starting point for running a Witness node

module Main where

import Control.Concurrent (threadDelay)
import Loot.Log (logInfo, logWarning, modifyLogName)
import Options.Applicative (execParser, fullDesc, helper, info, progDesc)
import UnliftIO.Async (async)

import Dscp.CLI (versionOption)
import Dscp.Config (buildBaseConfig, configPathParser)
import Dscp.Listeners (witnessListeners)
import Dscp.Network (runListener, runWorker, withServer)
import Dscp.Witness (WitnessConfig, WitnessParams, launchWitnessRealMode, witnessParamsParser)
import Dscp.Workers (witnessWorkers)


main :: IO ()
main = do
    (witnessParams, witnessConfig) <- getWitnessParams
    launchWitnessRealMode witnessConfig witnessParams $
        withServer $
        modifyLogName (<> "node") $ do
            logInfo "Starting node."

            logInfo "Forking workers"
            forM_ witnessWorkers $ void . async . runWorker identity

            logInfo "Forking listeners"
            forM_ witnessListeners $ void . async . runListener identity

            logInfo "All done"
            logInfo "Hey, here log-warper works!"
            logWarning "Don't forget to implement everything else though!"
            forever $ liftIO $ threadDelay 10000000

getWitnessParams :: IO (WitnessParams, WitnessConfig)
getWitnessParams = do
    let parser = (,) <$> witnessParamsParser <*> configPathParser
    (params, configPath) <- execParser $
        info (helper <*> versionOption <*> parser) $
        fullDesc <> progDesc "Disciplina witness node."
    config <- buildBaseConfig configPath
    return (params, config)
