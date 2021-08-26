{-# LANGUAGE OverloadedLabels #-}

-- | Witness entry point.

module Dscp.Witness.Launcher.Entry
    ( withWitnessBackground
    , witnessEntry
    ) where

import Control.Concurrent (threadDelay)
import Fmt ((+|), (|+))
import Loot.Log (logDebug, logInfo)
import UnliftIO.Async (async)

import Dscp.Config (sub, whenConfigJust)
import Dscp.Network
import Dscp.Witness.Config
import Dscp.Witness.Launcher.Context
import Dscp.Witness.Listeners
import Dscp.Witness.Logic.Init
import Dscp.Witness.Web
import Dscp.Witness.Workers

-- | Listeners, workers and no interaction with user.
withWitnessBackground :: FullWitnessWorkMode ctx m => m a -> m a
withWitnessBackground cont = do
    initStorage

    logInfo $ "Genesis header: " +| genesisHeader |+ ""

    logInfo "Forking witness workers and listeners"
    withWorkers witnessListeners . withWorkers witnessClients $
        cont

-- | Entry point of witness node.
witnessEntry :: FullWitnessWorkMode ctx m => m a
witnessEntry =
    withServer . withWitnessBackground $ do
        let mServerParams = witnessConfig ^. sub #witness . sub #api
        whenConfigJust mServerParams $ \serverParams -> do
            logInfo "Forking witness API server"
            void . async $
                serveWitnessAPIReal serverParams

        logDebug "Witness initialisation is complete"
        forever $ liftIO $ threadDelay 10000000
