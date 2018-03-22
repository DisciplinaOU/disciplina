
-- | Starting point for running a Witness node

module Workers where

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

import Messages




witnessTxWorker
    :: NodeId
    -> StdGen
    -> [NodeId]
    -> Converse Packing BS.ByteString  Production
    -> Production ()
witnessTxWorker anId generator peerIds = worker generator
    where
    worker
        :: StdGen
        -> Converse Packing BS.ByteString Production
        -> Production ()
    worker gen converse = loop gen
        where
        loop :: StdGen -> Production ()
        loop g = do
            let (i, gen') = randomR (0,1000000) g
                us = fromMicroseconds i :: Microsecond
            delay us
            let pong :: NodeId -> ConversationActions PingTx PongTx Production -> Production ()
                pong peerId cactions = do
                    liftIO . putStrLn $ show anId ++ " sent PING to " ++ show peerId
                    received <- recv cactions maxBound
                    case received of
                        Just (PongTx _) -> liftIO . putStrLn $ show anId ++ " heard PONG from " ++ show peerId
                        Nothing -> error "Unexpected end of input"
            _ <- forConcurrently peerIds $ \peerId ->
                converseWith converse peerId (\_ -> Conversation (pong peerId))
            loop gen'

witnessBlkWorker
    :: NodeId
    -> StdGen
    -> [NodeId]
    -> Converse Packing BS.ByteString Production
    -> Production ()
witnessBlkWorker anId generator peerIds = worker generator
    where
    worker
        :: StdGen
        -> Converse Packing BS.ByteString Production
        -> Production ()
    worker gen converse = loop gen
        where
        loop :: StdGen -> Production ()
        loop g = do
            let (i, gen') = randomR (0,1000000) g
                us = fromMicroseconds i :: Microsecond
            delay us
            let pong :: NodeId -> ConversationActions PingBlk PongBlk Production -> Production ()
                pong peerId cactions = do
                    liftIO . putStrLn $ show anId ++ " sent PING to " ++ show peerId
                    received <- recv cactions maxBound
                    case received of
                        Just (PongBlk _) -> liftIO . putStrLn $ show anId ++ " heard PONG from " ++ show peerId
                        Nothing -> error "Unexpected end of input"
            _ <- forConcurrently peerIds $ \peerId ->
                converseWith converse peerId (\_ -> Conversation (pong peerId))
            loop gen'

