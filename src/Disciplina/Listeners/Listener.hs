
-- | Node Listeners

module Disciplina.Listeners.Listener where

import Universum

import qualified Data.ByteString as BS
import Node (ConversationActions, Listener (..), NodeId, send)
import System.Wlog (logInfo)

import Disciplina.Launcher.Mode (BasicRealMode)
import Disciplina.Messages (Packing, PingBlk, PingTx, PongBlk (..), PongTx (..))

witnessListeners
    :: NodeId
    -> BS.ByteString
    -> [Listener Packing BS.ByteString BasicRealMode]
witnessListeners _anId _peerData = [blkListener, txListener]
    where
    blkListener :: Listener Packing BS.ByteString BasicRealMode
    blkListener = Listener $ \_ _peerId (cactions :: ConversationActions PongBlk PingBlk BasicRealMode) -> do
        logInfo "heard Blk"
        send cactions (PongBlk "")
    txListener :: Listener Packing BS.ByteString BasicRealMode
    txListener = Listener $ \_ _peerId (cactions :: ConversationActions PongTx PingTx BasicRealMode) -> do
        logInfo "heard Tx"
        send cactions (PongTx "")
