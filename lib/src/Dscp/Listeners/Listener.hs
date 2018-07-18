{-# LANGUAGE OverloadedLists #-}

-- | Node Listeners

module Dscp.Listeners.Listener
    ( witnessListeners
    ) where

import Loot.Log (logInfo)

import Dscp.Network.Messages (PingBlk (..), PingTx (..), PongBlk (..), PongTx (..))
import Dscp.Network.Wrapped (Listener, lcallback, msgType, servSend, simpleListener)
import Dscp.Witness.Launcher (WitnessWorkMode)


witnessListeners
    :: forall m. WitnessWorkMode m
    => [Listener m]
witnessListeners = [blkListener, txListener]
   where
     blkListener =
         simpleListener "blkListener" [msgType @PingBlk] $ \btq ->
         let blkCallback cId PingBlk = do
                 logInfo "got PingBlk"
                 atomically $ servSend btq cId (PongBlk "that was a great block")
                 logInfo "got PingBlk, replied"
         in [ lcallback blkCallback ]

     txListener =
         simpleListener "txListener" [msgType @PingTx] $ \btq ->
         let txCallback cId PingTx = do
                 logInfo "got PingTx"
                 atomically $ servSend btq cId (PongTx "wonderful tx, thank you!")
                 logInfo "got PingTx, replied"
         in [ lcallback txCallback ]
