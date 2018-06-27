-- | Starting point for running a Witness node

module Main where

import Universum

import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8 as B8
import Loot.Log (logInfo, logWarning, modifyLogName)
import Options.Applicative (Parser, execParser, fullDesc, help, helper, info, long, progDesc)
import System.IO (getChar)
import System.Random (mkStdGen)
import UnliftIO.Async (concurrently_)
import UnliftIO.Async (async)

import Dscp.CLI (versionOption)
import Dscp.Listeners (witnessListeners)
import Dscp.Network (runListener, runWorker, withServer)
import Dscp.Witness (WitnessParams, launchWitnessRealMode, witnessParamsParser)
import Dscp.Workers (witnessWorkers)


main :: IO ()
main = do
    witnessParams <- getWitnessParams
    launchWitnessRealMode witnessParams $
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

getWitnessParams :: IO WitnessParams
getWitnessParams =
    execParser $ info (helper <*> versionOption <*> witnessParamsParser) $
    fullDesc <> progDesc "Disciplina witness node."
