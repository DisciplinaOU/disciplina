-- | Witness entry point.

module Dscp.Witness.Launcher.Entry
    ( passiveWitnessEntry
    , witnessEntry
    ) where

import Control.Concurrent (threadDelay)
import Fmt ((+|), (|+))
import Loot.Base.HasLens (lensOf)
import Loot.Log (logInfo)
import UnliftIO.Async (async)

import Dscp.Network (runListener, runWorker, withServer)
import Dscp.Witness.Config
import Dscp.Witness.Launcher.Mode
import Dscp.Witness.Launcher.Params
import Dscp.Witness.Listeners
import Dscp.Witness.Logic
import Dscp.Witness.SDLock
import Dscp.Witness.Web
import Dscp.Witness.Workers

-- | Listeners, workers and no interaction with user.
passiveWitnessEntry :: WitnessWorkMode ctx m => m ()
passiveWitnessEntry = do
    -- this should be done only if resource is not initialised,
    -- and this call should be in SDActions allocation code, but
    -- now we always start with the empty state.
    writingSDLock "apply genesis block" applyGenesisBlock

    -- todo git revision
    logInfo $ "Genesis header: " +| genesisHeader |+ ""

    logInfo "Forking workers"
    witnessWorkers >>= mapM_ (void . async . runWorker identity)

    logInfo "Forking listeners"
    witnessListeners >>= mapM_ (void . async . runListener identity)

-- | Entry point of witness node.
witnessEntry :: WitnessWorkMode ctx m => m ()
witnessEntry =
    withServer $ do
        passiveWitnessEntry

        witnessParams <- view (lensOf @WitnessParams)
        whenJust (wpWalletServerParams witnessParams) $ \serverParams -> do
            logInfo "Forking witness API server"
            void . async $
                serveWitnessAPIReal serverParams

        logInfo "All done"
        forever $ liftIO $ threadDelay 10000000
