
-- | Node workers

module Disciplina.Workers.Worker where

import Universum

import           Mockable (Production (..))
import           System.Wlog (logInfo, logWarning)
import           Node
import qualified Data.ByteString as BS
import           System.Random
import           Mockable.Concurrent (delay, forConcurrently)
import           Data.Time.Units (Microsecond, fromMicroseconds)

import           Disciplina.Messages



witnessWorkers = [witnessTxWorker
                 ,witnessBlkWorker]

witnessTxWorker
    :: NodeId
    -> [NodeId]
    -> Converse Packing BS.ByteString Production
    -> Production ()
witnessTxWorker anId peerIds conv = logInfo "worker initialized" >> worker conv
    where
    worker
        :: Converse Packing BS.ByteString Production
        -> Production ()
    worker converse = loop
        where
        loop :: Production ()
        loop = do
            let pongTx :: NodeId -> ConversationActions PingTx PongTx Production -> Production ()
                pongTx peerId cactions = do
                    received <- recv cactions maxBound
                    case received of
                        Just (PongTx _) -> logInfo "heard Tx"
                        Nothing -> error "Unexpected end of input"
            -- _ <- forConcurrently peerIds $ \peerId ->
            --     converseWith converse peerId (\_ -> Conversation (pongTx peerId))
            loop

witnessBlkWorker
    :: NodeId
    -> [NodeId]
    -> Converse Packing BS.ByteString Production
    -> Production ()
witnessBlkWorker anId peerIds conv = logInfo "worker initialized" >> worker conv
    where
    worker
        :: Converse Packing BS.ByteString Production
        -> Production ()
    worker converse = loop
        where
        loop :: Production ()
        loop = do
            let pongBlk :: NodeId -> ConversationActions PingBlk PongBlk Production -> Production ()
                pongBlk peerId cactions = do
                    received <- recv cactions maxBound
                    case received of
                        Just (PongBlk _) -> logInfo "heard Blk"
                        Nothing -> error "Unexpected end of input"
            -- _ <- forConcurrently peerIds $ \peerId ->
            --     converseWith converse peerId (\_ -> Conversation (pongBlk peerId))
            loop

