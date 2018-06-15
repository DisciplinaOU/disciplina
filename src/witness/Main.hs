
-- | Starting point for running a Witness node

module Main where

import Universum

import System.Wlog (logInfo, logWarning)

import qualified Data.ByteString.Char8 as B8
import qualified Network.Transport.TCP as TCP
import Node (NodeAction (..), defaultNodeEnvironment, noReceiveDelay, node, nodeId,
             simpleNodeEndPoint)
import System.IO (getChar)
import System.Random (mkStdGen)
import UnliftIO.Async (async)

import Disciplina.DB (DBParams (..))
import Disciplina.Listeners (witnessListeners)
import Disciplina.Messages (serialisePacking)
import Disciplina.Transport (bracketTransportTCP)
import Disciplina.Witness (WitnessParams (..), launchWitnessRealMode)
import Disciplina.Workers (witnessWorkers)
import WitnessParams (WitnessOptParams (..), getWitnessParams)

main :: IO ()
main = do
    WitnessOptParams {..} <- getWitnessParams
    let witnessParams = WitnessParams
            { wpLoggingParams = wpLogParams
            , wpDBParams = DBParams{ dbpPath = wpDbPath }
            }
    launchWitnessRealMode witnessParams $ do
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
