
-- | Node workers

module Disciplina.Workers.Worker where

import Universum

import qualified Data.ByteString as BS
import Data.Time.Units (Microsecond, fromMicroseconds)
import Mockable (Production (..))
import Mockable.Concurrent (delay, forConcurrently)
import Node (NodeId, ConversationActions, Converse, recv)
import System.Wlog (logInfo, logWarning)

import Disciplina.Launcher.Mode (BasicRealMode)
import Disciplina.Messages (PongTx (..), PongBlk (..), PingTx (..), PingBlk (..),
                           Packing)



witnessWorkers = [witnessTxWorker
                 ,witnessBlkWorker]

witnessTxWorker
    :: NodeId
    -> [NodeId]
    -> Converse Packing BS.ByteString BasicRealMode
    -> BasicRealMode ()
witnessTxWorker anId peerIds conv = logInfo "tx worker initialized" >> worker conv
    where
    worker
        :: Converse Packing BS.ByteString BasicRealMode
        -> BasicRealMode ()
    worker converse = loop
        where
        loop :: BasicRealMode ()
        loop = do
            let pongTx :: NodeId -> ConversationActions PingTx PongTx BasicRealMode -> BasicRealMode ()
                pongTx peerId cactions = do
                    received <- recv cactions maxBound
                    case received of
                        Just (PongTx _) -> logInfo "heard Tx"
                        Nothing         -> error "Unexpected end of input"
            -- _ <- forConcurrently peerIds $ \peerId ->
            --     converseWith converse peerId (\_ -> Conversation (pongTx peerId))
            loop

witnessBlkWorker
    :: NodeId
    -> [NodeId]
    -> Converse Packing BS.ByteString BasicRealMode
    -> BasicRealMode ()
witnessBlkWorker anId peerIds conv = logInfo "blk worker initialized" >> worker conv
    where
    worker
        :: Converse Packing BS.ByteString BasicRealMode
        -> BasicRealMode ()
    worker converse = loop
        where
        loop :: BasicRealMode ()
        loop = do
            let pongBlk :: NodeId -> ConversationActions PingBlk PongBlk BasicRealMode -> BasicRealMode ()
                pongBlk peerId cactions = do
                    received <- recv cactions maxBound
                    case received of
                        Just (PongBlk _) -> logInfo "heard Blk"
                        Nothing          -> error "Unexpected end of input"
            -- _ <- forConcurrently peerIds $ \peerId ->
            --     converseWith converse peerId (\_ -> Conversation (pongBlk peerId))
            loop

