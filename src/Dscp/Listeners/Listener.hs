{-# LANGUAGE OverloadedLists #-}

-- | Node Listeners

module Dscp.Listeners.Listener
    ( witnessListeners
    ) where

import Universum

import qualified Data.ByteString.Lazy as BSL

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
