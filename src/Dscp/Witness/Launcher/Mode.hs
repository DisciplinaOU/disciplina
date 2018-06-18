{-# LANGUAGE TemplateHaskell #-}

-- | Module contains the definition of Witness's WorkMode and its implementations.

module Dscp.Witness.Launcher.Mode
    (
      -- * Constraints
      WitnessWorkMode

      -- * Implementations
    , WitnessContext (..)
    , WitnessRealMode
    , wcDB
    , wcLogging
    ) where

import Control.Lens (makeLenses)
import Ether.Internal (HasLens (..))
import Loot.Log.Rio (LoggingIO)

import Dscp.DB.Rocks.Class (MonadDB)
import Dscp.DB.Rocks.Real.Types (RocksDB)
import qualified Dscp.Launcher.Mode as Basic
import Dscp.Launcher.Rio (RIO)

---------------------------------------------------------------------
-- WorkMode class
---------------------------------------------------------------------

-- | Set of typeclasses which define capabilities of Witness node.
type WitnessWorkMode m =
    ( Basic.BasicWorkMode m
    , MonadDB m
    )

---------------------------------------------------------------------
-- WorkMode implementation
---------------------------------------------------------------------

data WitnessContext = WitnessContext
    { _wcDB      :: RocksDB
    , _wcLogging :: LoggingIO
    }

makeLenses ''WitnessContext

type WitnessRealMode = RIO WitnessContext

---------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------

instance HasLens RocksDB WitnessContext RocksDB where
    lensOf = wcDB

instance HasLens LoggingIO WitnessContext LoggingIO where
    lensOf = wcLogging