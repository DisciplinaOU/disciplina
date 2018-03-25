
-- | Starting point for running a Witness node

module Main where

import Universum

import Mockable (runProduction, Production (..))
import System.Wlog (logInfo, logWarning)

import Disciplina.Launcher (BasicNodeParams (..), LoggingParams (..), bracketBasicNodeResources,
                            runBasicRealMode)
import Disciplina.Listeners
import Disciplina.Workers
import Params (WitnessParams (..), getWitnessParams)

import qualified Network.Transport.TCP as TCP
--import           Pos.Diffusion.Transport.TCP (bracketTransportTCP)
import           Node
import           Node.Message.Binary (binaryPacking)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as BS
import           System.Random
import           Mockable.Concurrent (fork, killThread)
import           Network.Transport.Abstract (closeTransport, Transport)
import           Network.Transport.Concrete (concrete)
import           System.IO (getChar)



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

            --next to implement, use bracketTransportTCP
            --bracketTransportTCP networkConnectionTimeout tcpAddr $ \transport ->

            let params = TCP.defaultTCPParameters { TCP.tcpCheckPeerHost = True }
            transport_ <- do
                transportOrError <- liftIO $
                    TCP.createTransport (TCP.defaultTCPAddr "127.0.0.1" "10128") params
                either throwM return transportOrError
            let transport :: Transport Production
                transport = concrete transport_

            let prng1 = mkStdGen 0
            let prng2 = mkStdGen 1


            logInfo "Starting node"
            lift $ node (simpleNodeEndPoint transport) (const noReceiveDelay) (const noReceiveDelay)
                 prng1 binaryPacking (B8.pack "I am node 1") defaultNodeEnvironment $ \node1 ->
                        NodeAction (witnessListeners . nodeId $ node1) $ \converse -> do
                            tids <- mapM (\w -> fork $ w (nodeId node1) [] converse) witnessWorkers
                            logInfo "Hit return to stop"
                            _ <- liftIO getChar
                            mapM_ killThread tids
                            logInfo "Stopping node"
            logInfo "All done."
            lift $ closeTransport transport

            logInfo "Hey, here log-warper works!"
            logWarning "Don't forget to implement everything else though!"

