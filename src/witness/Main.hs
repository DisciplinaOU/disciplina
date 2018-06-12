
-- | Starting point for running a Witness node

module Main where

import Universum

import Mockable (Production (..), runProduction)
import System.Wlog (logInfo, logWarning)

import qualified Data.ByteString.Char8 as B8
import Mockable.Concurrent (fork)
import qualified Network.Transport.TCP as TCP
import Node (NodeAction (..), defaultNodeEnvironment, noReceiveDelay, node, nodeId,
             simpleNodeEndPoint)
import System.IO (getChar)
import System.Random (mkStdGen)

import Disciplina.DB (DBType (WitnessDB))
import Disciplina.Launcher (BasicNodeParams (..), bracketBasicNodeResources, runBasicRealMode)
import Disciplina.Listeners (witnessListeners)
import Disciplina.Messages (serialisePacking)
import Disciplina.Transport (bracketTransportTCP)
import Disciplina.Workers (witnessWorkers)
import WitnessParams (WitnessParams (..), getWitnessParams)

main :: IO ()
main = do
    WitnessParams {..} <- getWitnessParams
    let basicParams = BasicNodeParams
            { bnpLoggingParams = wpLogParams
            , bnpDBType        = WitnessDB
            , bnpDBPath        = wpDbPath
            }
    runProduction . bracketBasicNodeResources basicParams $
        \nr -> runBasicRealMode nr $
          bracketTransportTCP (15000 {-- connection timeout ms--})
                              (TCP.defaultTCPAddr "127.0.0.1" "10128") $ \transport -> do

            let prng1 = mkStdGen 0

            logInfo "Starting node"
            node (simpleNodeEndPoint transport) (const noReceiveDelay) (const noReceiveDelay)
                 prng1 serialisePacking (B8.pack "I am node 1") defaultNodeEnvironment $ \node1 ->
                        NodeAction (witnessListeners . nodeId $ node1) $ \converse -> do
                            mapM_ (\w -> fork $ w (nodeId node1) [] converse) witnessWorkers
                            logInfo "Hit return to stop"
                            _ <- liftIO getChar
                            logInfo "Stopping node"
            logInfo "All done."

            logInfo "Hey, here log-warper works!"
            logWarning "Don't forget to implement everything else though!"
