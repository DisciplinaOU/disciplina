{-# LANGUAGE OverloadedLabels #-}

-- | Witness entry point.

module Dscp.Witness.Launcher.Entry
    ( withWitnessBackground
    , witnessEntry
    ) where

import Control.Concurrent (threadDelay)
import Fmt ((+|), (|+))
import Loot.Config (option, sub)
import Loot.Log (logInfo)
import Time (sec)
import UnliftIO.Async (async, cancel)

import Dscp.Network (runListener, runWorker, withServer)
import Dscp.Util.TimeLimit
import Dscp.Witness.Config
import Dscp.Witness.Launcher.Mode
import Dscp.Witness.Listeners
import Dscp.Witness.Logic
import Dscp.Witness.SDLock
import Dscp.Witness.Web
import Dscp.Witness.Workers

-- | Listeners, workers and no interaction with user.
withWitnessBackground :: FullWitnessWorkMode ctx m => m () -> m ()
withWitnessBackground cont = do
    -- this should be done only if resource is not initialised,
    -- and this call should be in SDActions allocation code, but
    -- now we always start with the empty state.
    writingSDLock "apply genesis block" applyGenesisBlock

    -- todo git revision
    logInfo $ "Genesis header: " +| genesisHeader |+ ""

    workers <- witnessWorkers
    listeners <- witnessListeners

    mask $ \unmask -> do
        logInfo "Forking witness workers"
        workerAsyncs <- mapM (async . runWorker identity) workers

        logInfo "Forking witness listeners"
        listenerAsyncs <- mapM (async . runListener identity) listeners

        unmask cont
            `finally` mapM terminate (workerAsyncs <> listenerAsyncs)
  where
    terminate =
        logWarningWaitInf (sec 1) "Witness worker/listener shutdown" . cancel

-- | Entry point of witness node.
witnessEntry :: FullWitnessWorkMode ctx m => m ()
witnessEntry =
    withServer. withWitnessBackground $ do
        let mServerParams = witnessConfig ^. sub #witness . option #api
        whenJust mServerParams $ \serverParams -> do
            logInfo "Forking witness API server"
            void . async $
                serveWitnessAPIReal serverParams

        logInfo "All done"
        forever $ liftIO $ threadDelay 10000000
