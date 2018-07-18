{-# LANGUAGE OverloadedLists #-}

-- | Node workers

module Dscp.Workers.Worker
    ( witnessWorkers
    ) where

import Control.Concurrent (threadDelay)
import Fmt ((+|), (||+))
import Loot.Log (logError, logInfo)
import Loot.Network.Class (ClientEnv)
import Loot.Network.ZMQ (ZmqTcp)

import Dscp.Network.Messages (PingBlk (..), PingTx (..), PongBlk (..), PongTx (..))
import Dscp.Network.Wrapped (Worker (..), cliRecvResp, cliSend, msgType)
import Dscp.Witness.Launcher (WitnessWorkMode)

witnessWorkers :: WitnessWorkMode m => [Worker ZmqTcp m]
witnessWorkers = [witnessTxWorker, witnessBlkWorker]

witnessTxWorker :: forall m. WitnessWorkMode m => Worker ZmqTcp m
witnessTxWorker =
    Worker "txWorker" [msgType @PongTx] [] (\btq -> action btq `catchAny` handler)
  where
    handler e = logError $ fromString $ "Exception in txWorker: " <> show e
    action :: ClientEnv ZmqTcp -> m ()
    action btq = do
      logInfo "Started witness tx worker"
      forever $ do
        cliSend btq Nothing PingTx
        (nId,PongTx txt) <- cliRecvResp btq (-1)
        logInfo $ "Heard pongtx: " +| txt ||+ " from " +| nId ||+ ""
        liftIO $ threadDelay 1000000

witnessBlkWorker :: forall m. WitnessWorkMode m => Worker ZmqTcp m
witnessBlkWorker =
    Worker "blkWorker" [msgType @PongBlk] [] (\btq -> action btq `catchAny` handler)
  where
    handler e = logError $ fromString $ "Exception in txWorker: " <> show e
    action :: ClientEnv ZmqTcp -> m ()
    action btq = do
      liftIO $ threadDelay 500000 -- for clarity of wor
      logInfo "Started witness blk worker"
      forever $ do
        cliSend btq Nothing PingBlk
        (nId,PongBlk txt) <- cliRecvResp btq (-1)
        logInfo $ "Heard pongblk: " +| txt ||+ " from " +| nId ||+ ""
        liftIO $ threadDelay 1000000
