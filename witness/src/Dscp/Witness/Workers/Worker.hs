{-# LANGUAGE OverloadedLists #-}

-- | Node workers

module Dscp.Witness.Workers.Worker
    ( witnessWorkers
    ) where

import Fmt ((+|), (+||), (|+), (||+))
import Loot.Log (logError, logInfo)

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
        unless (tip == hash (bHeader block)) $ do
            logInfo "Block is new, applying"
            proof <- applyBlock block
            logInfo $ "Applied received block: " +| block |+
                      "with proof" +|| proof ||+ ", propagating"
