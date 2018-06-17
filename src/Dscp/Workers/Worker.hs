-- | Node workers

module Dscp.Workers.Worker where

import Universum

import Loot.Network.Class (ClientEnv, ClientId, MsgType, Subscription)

import Dscp.Witness.Launcher (WitnessWorkMode)


data ClientWorker t m = ClientWorker
    { cliId          :: ClientId
    , cMsgTypes      :: Set MsgType
    , sSubscriptions :: Set Subscription
    , cAction        :: ClientEnv t -> m ()
    }

witnessWorkers :: WitnessWorkMode m => [ClientWorker t m]
witnessWorkers = [] -- [ witnessTxWorker
                    -- , witnessBlkWorker
                    -- ]

-- TODO DSCP-105 Repair this
--witnessTxWorker :: forall m. WitnessWorkMode m => WitnessWorker m
--witnessTxWorker _anId peerIds conv = logInfo "tx worker initialized" >> worker conv
--    where
--    worker
--        :: Converse Packing ByteString m
--        -> m ()
--    worker converse = loop
--        where
--        loop :: m ()
--        loop = do
--            let pongTx :: NodeId -> ConversationActions PingTx PongTx m -> m ()
--                pongTx _peerId cactions = do
--                    received <- recv cactions maxBound
--                    case received of
--                        Just (PongTx _) -> logInfo "heard Tx"
--                        Nothing         -> error "Unexpected end of input"
--            forConcurrently_ peerIds $ \peerId ->
--                converseWith converse peerId (\_ -> Conversation (pongTx peerId))
--            loop
--
--witnessBlkWorker :: forall m. WitnessWorkMode m => WitnessWorker m
--witnessBlkWorker _anId peerIds conv = logInfo "blk worker initialized" >> worker conv
--    where
--    worker
--        :: Converse Packing ByteString m
--        -> m ()
--    worker converse = loop
--        where
--        loop :: m ()
--        loop = do
--            let pongBlk :: NodeId -> ConversationActions PingBlk PongBlk m -> m ()
--                pongBlk _peerId cactions = do
--                    received <- recv cactions maxBound
--                    case received of
--                        Just (PongBlk _) -> logInfo "heard Blk"
--                        Nothing          -> error "Unexpected end of input"
--            forConcurrently_ peerIds $ \peerId ->
--                converseWith converse peerId (\_ -> Conversation (pongBlk peerId))
--            loop
