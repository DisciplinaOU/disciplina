{-# LANGUAGE OverloadedLists #-}

-- | Node Listeners

module Dscp.Listeners.Listener
    ( witnessListeners
    ) where

import Control.Concurrent (threadDelay)
import Loot.Log (logInfo)

import Dscp.Network.Messages (PingBlk (..), PingTx (..), PongBlk (..), PongTx (..))
import Dscp.Network.Wrapped (Listener, lcallback, msgType, servSend, simpleListener)
import Dscp.Witness.Launcher (WitnessWorkMode)


witnessListeners
    :: forall m t. WitnessWorkMode m
    => [Listener t m]
witnessListeners = [blkListener, txListener]
   where
     blkListener =
         simpleListener "blkListener" [msgType @PingBlk] $ \btq ->
         let blkCallback cId PingBlk = do
                 logInfo "got PingBlk"
                 atomically $ servSend @t btq cId (PongBlk "that was a great block")
                 logInfo "got PingBlk, replied"
         in [ lcallback @t blkCallback ]

     txListener =
         simpleListener "txListener" [msgType @PingTx] $ \btq ->
         let txCallback cId PingTx = do
                 logInfo "got PingTx"
                 atomically $ servSend @t btq cId (PongTx "wonderful tx, thank you!")
                 logInfo "got PingTx, replied"
         in [ lcallback @t txCallback ]
