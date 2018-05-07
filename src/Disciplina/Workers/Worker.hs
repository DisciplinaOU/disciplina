
-- | Node workers

module Disciplina.Workers.Worker where

import Universum

import Mockable.Concurrent (forConcurrently)
import Node (Conversation (..), ConversationActions, Converse, NodeId, converseWith, recv)
import System.Wlog (logInfo)

import Disciplina.Launcher.Mode (BasicRealMode)
import Disciplina.Messages (Packing, PingBlk (..), PingTx (..), PongBlk (..), PongTx (..))

-- | Function which accepts current node ID and IDs of peers, a conversation object
-- and starts doing some network stuff.
type WitnessWorker =
       NodeId
    -> [NodeId]
    -> Converse Packing ByteString BasicRealMode
    -> BasicRealMode ()

witnessWorkers :: [WitnessWorker]
witnessWorkers = [ witnessTxWorker
                 , witnessBlkWorker
                 ]

witnessTxWorker :: WitnessWorker
witnessTxWorker _anId peerIds conv = logInfo "tx worker initialized" >> worker conv
    where
    worker
        :: Converse Packing ByteString BasicRealMode
        -> BasicRealMode ()
    worker converse = loop
        where
        loop :: BasicRealMode ()
        loop = do
            let pongTx :: NodeId -> ConversationActions PingTx PongTx BasicRealMode -> BasicRealMode ()
                pongTx _peerId cactions = do
                    received <- recv cactions maxBound
                    case received of
                        Just (PongTx _) -> logInfo "heard Tx"
                        Nothing         -> error "Unexpected end of input"
            _ <- forConcurrently peerIds $ \peerId ->
                converseWith converse peerId (\_ -> Conversation (pongTx peerId))
            loop

witnessBlkWorker :: WitnessWorker
witnessBlkWorker _anId peerIds conv = logInfo "blk worker initialized" >> worker conv
    where
    worker
        :: Converse Packing ByteString BasicRealMode
        -> BasicRealMode ()
    worker converse = loop
        where
        loop :: BasicRealMode ()
        loop = do
            let pongBlk :: NodeId -> ConversationActions PingBlk PongBlk BasicRealMode -> BasicRealMode ()
                pongBlk _peerId cactions = do
                    received <- recv cactions maxBound
                    case received of
                        Just (PongBlk _) -> logInfo "heard Blk"
                        Nothing          -> error "Unexpected end of input"
            _ <- forConcurrently peerIds $ \peerId ->
                converseWith converse peerId (\_ -> Conversation (pongBlk peerId))
            loop

