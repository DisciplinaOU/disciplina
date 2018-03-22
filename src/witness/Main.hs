
-- | Starting point for running a Witness node

module Main where

import Universum

import           Data.Binary (Binary)
import Mockable (runProduction, Production (..))
import System.Wlog (logInfo, logWarning)

import Disciplina.Launcher (BasicNodeParams (..), LoggingParams (..), bracketBasicNodeResources,
                            runBasicRealMode)
import Params (WitnessParams (..), getWitnessParams)

import qualified Network.Transport.TCP as TCP
import           Node
import           Data.Data (Data)
import           Node.Message.Binary (BinaryP, binaryPacking)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as BS
import           System.Random
import           Mockable.Concurrent (delay, forConcurrently, fork, killThread)
import           Network.Transport.Abstract (closeTransport, Transport)
import           Network.Transport.Concrete (concrete)
import           Data.Time.Units (Microsecond, fromMicroseconds)
import           System.IO (getChar)

import Listeners
import Workers


main :: IO ()
main = do
    WitnessParams {..} <- getWitnessParams
    let loggingParams = LoggingParams
            { lpDefaultName = "witness"
            , lpDirectory   = wpLogDir
            , lpConfigPath  = wpLogConfig
            }
        basicParams = BasicNodeParams loggingParams
    runProduction . bracketBasicNodeResources basicParams $
        \nr -> runBasicRealMode nr $ do
            let params = TCP.defaultTCPParameters { TCP.tcpCheckPeerHost = True }
            transport_ <- do
                transportOrError <- liftIO $
                    TCP.createTransport (TCP.defaultTCPAddr "127.0.0.1" "10128") params
                either throwM return transportOrError
            let transport :: Transport Production
                transport = concrete transport_

            let prng1 = mkStdGen 0
            let prng2 = mkStdGen 1
            let prng3 = mkStdGen 2
            let prng4 = mkStdGen 3

            logInfo "Starting nodes"
            lift $ node (simpleNodeEndPoint transport) (const noReceiveDelay) (const noReceiveDelay)
                 prng1 binaryPacking (B8.pack "I am node 1") defaultNodeEnvironment $ \node1 ->
                NodeAction (witnessListeners . nodeId $ node1) $ \converse1 -> do
                    node (simpleNodeEndPoint transport) (const noReceiveDelay) (const noReceiveDelay)
                          prng2 binaryPacking (B8.pack "I am node 2") defaultNodeEnvironment $ \node2 ->
                        NodeAction (witnessListeners . nodeId $ node2) $ \converse2 -> do
                            tid1 <- fork $ witnessTxWorker (nodeId node1) prng3 [nodeId node2] converse1
                            tid2 <- fork $ witnessTxWorker (nodeId node2) prng4 [nodeId node1] converse2
                            logInfo "Hit return to stop"
                            _ <- liftIO getChar
                            killThread tid1
                            killThread tid2
                            logInfo "Stopping nodes"
            logInfo "All done."
            lift $ closeTransport transport

            logInfo "Hey, here log-warper works!"
            logWarning "Don't forget to implement everything else though!"

