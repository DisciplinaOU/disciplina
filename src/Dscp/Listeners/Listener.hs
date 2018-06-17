-- | Node Listeners

module Dscp.Listeners.Listener where

import Universum

import Loot.Network.Class (ListenerEnv, ListenerId, MsgType)

import Dscp.Witness.Launcher (WitnessWorkMode)

data Listener t m = Listener
    { lId       :: ListenerId
    , lMsgTypes :: Set MsgType
    , lAction   :: ListenerEnv t -> m ()
    }

-- DSCP-105 TODO
witnessListeners
    :: forall m t. WitnessWorkMode m
    => [Listener t m]
witnessListeners = []
--    where
--    blkListener :: Listener Packing BS.ByteString m
--    blkListener = Listener $ \_ _peerId (cactions :: ConversationActions PongBlk PingBlk m) -> do
--        logInfo "heard Blk"
--        send cactions (PongBlk "")
--    txListener :: Listener Packing BS.ByteString m
--    txListener = Listener $ \_ _peerId (cactions :: ConversationActions PongTx PingTx m) -> do
--        logInfo "heard Tx"
--        send cactions (PongTx "")
