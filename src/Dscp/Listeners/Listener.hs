{-# LANGUAGE OverloadedLists #-}

-- | Node Listeners

module Dscp.Listeners.Listener where

import Universum

import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BSL
import Loot.Network.BiTQueue (BiTQueue, recvBtq, sendBtq)
import Loot.Network.Class (CliId, Content, ListenerEnv, ListenerId, MsgType, ServSendMsg (..))
import Loot.Network.Message (CallbackWrapper (..), Message, handlerDecoded, runCallbacksInt)

import Dscp.Network.Messages (PingBlk (..), PingTx (..), PongBlk (..), PongTx (..))
import Dscp.Network.Wrapped (Listener, fromMsgType, lcallback, msgType, servSend, simpleListener)
import Dscp.Witness.Launcher (WitnessWorkMode)


witnessListeners
    :: forall m t. WitnessWorkMode m
    => [Listener t m]
witnessListeners = [blkListener, txListener]
   where
     blkListener =
         simpleListener "blkListener" [msgType @PingBlk] $ \btq ->
         let blkCallback cId PingBlk =
                 atomically $ servSend @t btq cId (PongBlk "that was a great block")
         in [ lcallback @t blkCallback ]

     txListener =
         simpleListener "txListener" [msgType @PingTx] $ \btq ->
         let txCallback cId PingTx =
                 atomically $ servSend @t btq cId (PongTx "wonderful tx, thank you!")
         in [ lcallback @t txCallback ]
