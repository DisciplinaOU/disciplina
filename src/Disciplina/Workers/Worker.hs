-- | Node workers

module Disciplina.Workers.Worker where

import Universum

import UnliftIO.Async (forConcurrently_)

import Node (Conversation (..), ConversationActions, Converse, NodeId, converseWith, recv)
import System.Wlog (logInfo)

import Disciplina.Launcher.Mode (WitnessWorkMode)
import Disciplina.Messages (Packing, PingBlk (..), PingTx (..), PongBlk (..), PongTx (..))

-- | Function which accepts current node ID and IDs of peers, a conversation object
-- and starts doing some network stuff.
type WitnessWorker m =
       NodeId
    -> [NodeId]
    -> Converse Packing ByteString m
    -> m ()

witnessWorkers :: WitnessWorkMode m => [WitnessWorker m]
witnessWorkers = [ witnessTxWorker
                 , witnessBlkWorker
                 ]

witnessTxWorker :: forall m. WitnessWorkMode m => WitnessWorker m
witnessTxWorker _anId peerIds conv = logInfo "tx worker initialized" >> worker conv
    where
    worker
        :: Converse Packing ByteString m
        -> m ()
    worker converse = loop
        where
        loop :: m ()
        loop = do
            let pongTx :: NodeId -> ConversationActions PingTx PongTx m -> m ()
                pongTx _peerId cactions = do
                    received <- recv cactions maxBound
                    case received of
                        Just (PongTx _) -> logInfo "heard Tx"
                        Nothing         -> error "Unexpected end of input"
            forConcurrently_ peerIds $ \peerId ->
                converseWith converse peerId (\_ -> Conversation (pongTx peerId))
            loop

witnessBlkWorker :: forall m. WitnessWorkMode m => WitnessWorker m
witnessBlkWorker _anId peerIds conv = logInfo "blk worker initialized" >> worker conv
    where
    worker
        :: Converse Packing ByteString m
        -> m ()
    worker converse = loop
        where
        loop :: m ()
        loop = do
            let pongBlk :: NodeId -> ConversationActions PingBlk PongBlk m -> m ()
                pongBlk _peerId cactions = do
                    received <- recv cactions maxBound
                    case received of
                        Just (PongBlk _) -> logInfo "heard Blk"
                        Nothing          -> error "Unexpected end of input"
            forConcurrently_ peerIds $ \peerId ->
                converseWith converse peerId (\_ -> Conversation (pongBlk peerId))
            loop

