
-- | Node Listeners

module Disciplina.Listeners.Listener where

import Universum

import Mockable (Production (..))
import System.Wlog (logInfo, logWarning)

import qualified Data.ByteString as BS
import Node (Listener (..), ConversationActions, NodeId, send)

import Disciplina.Launcher.Mode (BasicRealMode)
import Disciplina.Messages (PongTx (..), PongBlk (..), PingTx, PingBlk, Packing)



witnessListeners
    :: NodeId
    -> BS.ByteString
    -> [Listener Packing BS.ByteString BasicRealMode]
witnessListeners anId peerData = [blkListener, txListener]
    where
    blkListener :: Listener Packing BS.ByteString BasicRealMode
    blkListener = Listener $ \_ peerId (cactions :: ConversationActions PongBlk PingBlk BasicRealMode) -> do
        logInfo "heard Blk"
        send cactions (PongBlk "")
    txListener :: Listener Packing BS.ByteString BasicRealMode
    txListener = Listener $ \_ peerId (cactions :: ConversationActions PongTx PingTx BasicRealMode) -> do
        logInfo "heard Tx"
        send cactions (PongTx "")
