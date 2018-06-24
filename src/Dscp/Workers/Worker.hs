{-# LANGUAGE OverloadedLists #-}

-- | Node workers

module Dscp.Workers.Worker
    ( witnessWorkers
    ) where

import Universum

import Loot.Log (logInfo)
import Loot.Network.Class (ClientEnv)
import Loot.Network.ZMQ (ZmqTcp)

import Dscp.Network.Messages (PingBlk (..), PingTx (..), PongBlk (..), PongTx (..))
import Dscp.Network.Wrapped (Worker (..), cliRecvResp, cliSend, msgType)
import Dscp.Witness.Launcher (WitnessWorkMode)

witnessWorkers :: WitnessWorkMode m => [Worker ZmqTcp m]
witnessWorkers = [witnessTxWorker]

witnessTxWorker :: forall m. WitnessWorkMode m => Worker ZmqTcp m
witnessTxWorker = Worker "txWorker" [msgType @PongTx] [] action
  where
    action :: ClientEnv ZmqTcp -> m ()
    action btq = do
      logInfo "Started witness tx worker"
      forever $ do
        logInfo "txWorker: sending"
        cliSend btq Nothing PingTx
        logInfo "txWorker: receiving"
        (nId,PongTx txt) <- cliRecvResp btq (-1)
        logInfo $ fromString $ "Heard pongtx: " <> show txt <> " from " <> show nId

witnessBlkWorker :: forall m. WitnessWorkMode m => Worker ZmqTcp m
witnessBlkWorker = Worker "blkWorker" [msgType @PongBlk] [] action
  where
    action :: ClientEnv ZmqTcp -> m ()
    action btq = do
      logInfo "Started witness blk worker"
      forever $ do
        cliSend btq Nothing PingBlk
        (nId,PongBlk txt) <- cliRecvResp btq (-1)
        logInfo $ fromString $ "Heard pongblk: " <> show txt <> " from " <> show nId
