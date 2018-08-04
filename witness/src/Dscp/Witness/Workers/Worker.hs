{-# LANGUAGE OverloadedLists #-}

-- | Node workers

module Dscp.Witness.Workers.Worker
    ( witnessWorkers
    ) where

import Control.Concurrent (threadDelay)
import Fmt ((+|), (+||), (|+), (||+))
import Loot.Log (logError, logInfo)
import Loot.Network.Class (ClientEnv)
import Loot.Network.ZMQ (ZmqTcp)

import Dscp.Core
import Dscp.Crypto
import Dscp.Network.Wrapped
import Dscp.Snowdrop.Mode
import Dscp.Witness.Launcher.Mode
import Dscp.Witness.Logic
import Dscp.Witness.Messages


witnessWorkers :: WitnessWorkMode ctx m => [Worker m]
witnessWorkers = [blockReceivalWorker]

----------------------------------------------------------------------------
-- Updates
----------------------------------------------------------------------------

blockReceivalWorker :: forall ctx m. WitnessWorkMode ctx m => Worker m
blockReceivalWorker =
    Worker "blockIssuingListener" [] [subType @PubBlock] (\btq -> action btq `catchAny` handler)
  where
    handler e = logError $ fromString $ "Exception in blockReceivalWorker " <> show e
    action :: ClientEnv NetTag -> m ()
    action btq = forever $ do
        (_nId, PubBlock block) <- cliRecvUpdate btq (-1)
        logInfo $ "Received a new block: " +| hashF (headerHash block) |+ ""
        tip <- runSdM getTipHash
        unless (tip == hash (rbHeader block)) $ do
            logInfo "Block is new, applying"
            proof <- applyBlock block
            logInfo $ "Applied received block: " +| block |+
                      "with proof" +|| proof ||+ ", propagating"

----------------------------------------------------------------------------
-- Ping/pong workers
----------------------------------------------------------------------------

_witnessTxWorker :: forall ctx m. WitnessWorkMode ctx m => Worker m
_witnessTxWorker =
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

_witnessBlkWorker :: forall ctx m. WitnessWorkMode ctx m => Worker m
_witnessBlkWorker =
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
