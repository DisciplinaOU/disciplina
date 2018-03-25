
-- | Node Listeners

module Disciplina.Listeners.Listener where

import           Universum

import           Mockable (Production (..))
import           System.Wlog (logInfo, logWarning)

import           Node
import qualified Data.ByteString as BS

import           Disciplina.Messages



witnessListeners
    :: NodeId
    -> BS.ByteString
    -> [Listener Packing BS.ByteString Production]
witnessListeners anId peerData = [blkListener, txListener]
    where
    blkListener :: Listener Packing BS.ByteString Production
    blkListener = Listener $ \_ peerId (cactions :: ConversationActions PongBlk PingBlk Production) -> do
        logInfo "heard Blk"
        send cactions (PongBlk "")
    txListener :: Listener Packing BS.ByteString Production
    txListener = Listener $ \_ peerId (cactions :: ConversationActions PongTx PingTx Production) -> do
        logInfo "heard Tx"
        send cactions (PongTx "")
