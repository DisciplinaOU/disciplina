{-# LANGUAGE TemplateHaskell #-}

-- | Module contains the definition of Witness's WorkMode and its implementations.

module Dscp.Witness.Launcher.Mode
    (
      -- * Constraints
      WitnessWorkMode

      -- * Implementations
    , WitnessContext (..)
    , wcResources

    , WitnessRealMode
    ) where

import Universum

import Control.Lens (makeLenses)
import Loot.Base.HasLens (HasLens (..))
import Loot.Log.Rio (LoggingIO)
import Loot.Network.Class (NetworkingCli, NetworkingServ)
import Loot.Network.ZMQ as Z

import Dscp.DB.Rocks.Class (MonadDB)
import Dscp.DB.Rocks.Real.Types (RocksDB)
import qualified Dscp.Launcher.Mode as Basic
import Dscp.Launcher.Rio (RIO)
import Dscp.Network ()
import Dscp.Witness.Launcher.Resource (WitnessResources)

---------------------------------------------------------------------
-- WorkMode class
---------------------------------------------------------------------

-- | Set of typeclasses which define capabilities of Witness node.
type WitnessWorkMode m =
    ( Basic.BasicWorkMode m
    , MonadDB m
    , NetworkingCli ZmqTcp m
    , NetworkingServ ZmqTcp m
    )

---------------------------------------------------------------------
-- WorkMode implementation
---------------------------------------------------------------------

-- | Context is resources plus some runtime variables.
data WitnessContext = WitnessContext
    { _wcResources  :: WitnessResources
    }

makeLenses ''WitnessContext

type WitnessRealMode = RIO WitnessContext

---------------------------------------------------------------------
-- HasLens
---------------------------------------------------------------------

instance HasLens LoggingIO WitnessContext LoggingIO where
    lensOf = wcResources . lensOf @LoggingIO
instance HasLens RocksDB WitnessContext RocksDB where
    lensOf = wcResources . lensOf @RocksDB
instance HasLens Z.ZTGlobalEnv WitnessContext Z.ZTGlobalEnv where
    lensOf = wcResources . lensOf @Z.ZTGlobalEnv
instance HasLens Z.ZTNetCliEnv WitnessContext Z.ZTNetCliEnv where
    lensOf = wcResources . lensOf @Z.ZTNetCliEnv
instance HasLens Z.ZTNetServEnv WitnessContext Z.ZTNetServEnv where
    lensOf = wcResources . lensOf @Z.ZTNetServEnv

----------------------------------------------------------------------------
-- Sanity check
----------------------------------------------------------------------------

_sanity :: WitnessRealMode ()
_sanity = _sanityCallee
  where
    _sanityCallee :: WitnessWorkMode m => m ()
    _sanityCallee = pass
