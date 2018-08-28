-- | Storage (rocksdb) initialisation.

module Dscp.Witness.Logic.Init
    ( initStorage
    ) where

import Loot.Log (logDebug)

import Dscp.DB.CanProvideDB as DB
import Dscp.Witness.Launcher.Context
import Dscp.Witness.Logic.Apply
import Dscp.Witness.SDLock

-- | Storage initialisation. Does nothing if it was initialised
-- already.
initStorage :: forall ctx m. WitnessWorkMode ctx m => m ()
initStorage = do
    plugin <- providePlugin
    unlessM (isInitialised plugin) $ do
        writingSDLock "apply genesis block" applyGenesisBlock
        DB.pBatch plugin [DB.Put key ()]
        logDebug "RocksDB has been initialised"
  where
    key :: String
    key = "initialised"

    isInitialised plugin = do
        res :: Maybe () <- DB.pGet plugin key
        return $ isJust res
