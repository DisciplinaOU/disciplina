-- | Starting point for running a Witness node

module Main where

import Universum

import qualified Data.ByteString.Char8 as B8
import Loot.Log (logInfo, logWarning, modifyLogName)
import qualified Network.Transport.TCP as TCP
import Node (NodeAction (..), defaultNodeEnvironment, noReceiveDelay, node, nodeId,
             simpleNodeEndPoint)
import Options.Applicative (Parser, execParser, fullDesc, help, helper, info, long, progDesc)
import System.IO (getChar)
import System.Random (mkStdGen)
import UnliftIO.Async (async)

import Dscp.CLI (versionOption)
import Dscp.Listeners (witnessListeners)
import Dscp.Messages (serialisePacking)
import Dscp.Transport (bracketTransportTCP)
import Dscp.Witness (WitnessParams, launchWitnessRealMode, witnessParamsParser)
import Dscp.Workers (witnessWorkers)


main :: IO ()
main = do
    witnessParams <- getWitnessParams
    launchWitnessRealMode witnessParams $ do
      modifyLogName (<> "node") $ do
            logInfo "Starting node"
            -- TODO: This networking can't live without Production and Mockables
            --       so leaving it commented for now
            -- node (simpleNodeEndPoint transport) (const noReceiveDelay) (const noReceiveDelay)
            --      prng1 serialisePacking (B8.pack "I am node 1") defaultNodeEnvironment $ \node1 ->
            --             NodeAction (witnessListeners . nodeId $ node1) $ \converse -> do
            --                 mapM (\w -> async $ w (nodeId node1) [] converse) witnessWorkers
            --                 logInfo "Hit return to stop"
            --                 _ <- liftIO getChar
            --                 logInfo "Stopping node"
            logInfo "All done."

            logInfo "Hey, here log-warper works!"
            logWarning "Don't forget to implement everything else though!"


getWitnessParams :: IO WitnessParams
getWitnessParams =
    execParser $ info (helper <*> versionOption <*> witnessParamsParser) $
    fullDesc <> progDesc "Disciplina witness node."
