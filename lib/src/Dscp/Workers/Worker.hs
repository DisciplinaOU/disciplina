{-# LANGUAGE OverloadedLists #-}

-- | Node workers

module Dscp.Workers.Worker
    ( witnessWorkers
    ) where

import Control.Concurrent (threadDelay)
import Fmt ((+||), (||+))
import Loot.Log (logError, logInfo)
import Loot.Network.Class (ClientEnv)
import Loot.Network.ZMQ (ZmqTcp)

import Dscp.Network.Messages (PingBlk (..), PingTx (..), PongBlk (..), PongTx (..))
import Dscp.Network.Wrapped (Worker (..), cliRecvResp, cliSend, msgType)
import Dscp.Slotting (waitUntilNextSlot)
import Dscp.Witness.Block.Logic (applyBlock, createBlock)
import Dscp.Witness.Launcher (WitnessWorkMode)


witnessWorkers :: WitnessWorkMode ctx m => [Worker m]
witnessWorkers = [blockIssuingWorker, witnessTxWorker, witnessBlkWorker]

----------------------------------------------------------------------------
-- Block creation
----------------------------------------------------------------------------

blockIssuingWorker :: forall ctx m. WitnessWorkMode ctx m => Worker m
blockIssuingWorker =
    Worker "blockIssuingWorker" [] [] (\btq -> action btq `catchAny` handler)
  where
    handler e = logError $ fromString $ "Exception in blockIssuingWorker: " <> show e
    action :: ClientEnv ZmqTcp -> m ()
    action _btq = forever $ do
        slotId <- waitUntilNextSlot
        logInfo $ "New slot has just started: " +|| slotId ||+ ""
        block <- createBlock
        logInfo "Created a new block"
        proof <- applyBlock block
        logInfo $ "Applied block, proof: " +|| proof ||+ ""

----------------------------------------------------------------------------
-- Pinging
----------------------------------------------------------------------------


witnessTxWorker :: forall ctx m. WitnessWorkMode ctx m => Worker m
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
        logInfo $ "Heard pongtx: " +|| txt ||+ " from " +|| nId ||+ ""
        liftIO $ threadDelay 1000000

witnessBlkWorker :: forall ctx m. WitnessWorkMode ctx m => Worker m
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
        logInfo $ "Heard pongblk: " +|| txt ||+ " from " +|| nId ||+ ""
        liftIO $ threadDelay 1000000
