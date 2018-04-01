
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
import           Disciplina.Launcher.Mode (BasicRealMode)



witnessWorkers = [witnessTxWorker
                 ,witnessBlkWorker]

witnessTxWorker
    :: NodeId
    -> [NodeId]
    -> Converse Packing BS.ByteString BasicRealMode
    -> BasicRealMode ()
witnessTxWorker anId peerIds conv = logInfo "worker initialized" >> worker conv
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
                        Nothing -> error "Unexpected end of input"
            -- _ <- forConcurrently peerIds $ \peerId ->
            --     converseWith converse peerId (\_ -> Conversation (pongTx peerId))
            loop

witnessBlkWorker
    :: NodeId
    -> [NodeId]
    -> Converse Packing BS.ByteString BasicRealMode
    -> BasicRealMode ()
witnessBlkWorker anId peerIds conv = logInfo "worker initialized" >> worker conv
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
                        Nothing -> error "Unexpected end of input"
            -- _ <- forConcurrently peerIds $ \peerId ->
            --     converseWith converse peerId (\_ -> Conversation (pongBlk peerId))
            loop

