{-# LANGUAGE TemplateHaskell #-}

-- | Module contains the definition of Witness's WorkMode and its implementations.

module Dscp.Witness.Launcher.Mode
    (
      -- * Markers
      WitnessNode

      -- * Constraints
    , WitnessWorkMode
    , FullWitnessWorkMode

      -- * Implementations
    , WitnessContext (..)
    , wcResources

      -- * RealMode
    , WitnessRealMode

    , -- to avoid importing snowdrop imn Educator
      SD.OSParamsBuilder
    ) where

import Control.Lens (makeLenses)

import Loot.Base.HasLens (HasLens (..), HasLens')
import Loot.Log.Rio (LoggingIO)
import Loot.Network.Class (NetworkingCli, NetworkingServ)
import Loot.Network.ZMQ as Z
import qualified Snowdrop.Block as SD

import Dscp.DB.Rocks.Real.Types (RocksDB)
import qualified Dscp.Launcher.Mode as Basic
import Dscp.Network ()
import Dscp.Resource.Keys (KeyResources)
import Dscp.Rio (RIO)
import Dscp.Snowdrop.Actions (SDVars)
import Dscp.Web.Metrics
import Dscp.Witness.Config (HasWitnessConfig, withWitnessConfig)
import Dscp.Witness.Launcher.Marker (WitnessNode)
import Dscp.Witness.Launcher.Params
import Dscp.Witness.Launcher.Resource (WitnessResources, wrDB, wrKey, wrLogging, wrNetwork)
import Dscp.Witness.Mempool.Type (MempoolVar)
import Dscp.Witness.Relay (RelayState)
import Dscp.Witness.SDLock (SDLock)

---------------------------------------------------------------------
-- WorkMode class
---------------------------------------------------------------------

-- | Set of typeclasses which define capabilities of Witness node suitable for
-- most part of logic.
-- This excludes networking because emulating networking for tests is unpleasant
-- and actally only listeners/workers require it.
type WitnessWorkMode ctx m =
    ( Basic.BasicWorkMode m
    -- , MonadDB m  -- this will be replaced in PR which actually puts us on RocksDB

    , HasWitnessConfig

    , MonadReader ctx m

    , HasLens' ctx (Maybe WitnessWebParams)
    , HasLens' ctx MetricsEndpoint
    , HasLens' ctx LoggingIO
    , HasLens' ctx MempoolVar
    , HasLens' ctx SDVars
    , HasLens' ctx (KeyResources WitnessNode)
    , HasLens' ctx RelayState
    , HasLens' ctx SDLock
    )

type NetworkMode ctx m =
    ( NetworkingCli ZmqTcp m
    , NetworkingServ ZmqTcp m

    , HasLens' ctx Z.ZTGlobalEnv
    , HasLens' ctx Z.ZTNetCliEnv
    , HasLens' ctx Z.ZTNetServEnv
    )

-- | Full set of typeclasses which define capabilities of Witness node.
type FullWitnessWorkMode ctx m =
    ( WitnessWorkMode ctx m
    , NetworkMode ctx m
    )

---------------------------------------------------------------------
-- WorkMode implementation
---------------------------------------------------------------------

-- | Context is resources plus some runtime variables.
data WitnessContext = WitnessContext
    { _wcParams     :: !WitnessParams     -- ^ Parameters witness was started with.
    , _wcResources  :: !WitnessResources  -- ^ Resources, allocated from params.
    , _wcMempool    :: !MempoolVar
    , _wcSDActions  :: !SDVars
    , _wcRelayState :: !RelayState
    , _wcSDLock     :: !SDLock
    }


makeLenses ''WitnessContext

type WitnessRealMode = RIO WitnessContext

---------------------------------------------------------------------
-- HasLens
---------------------------------------------------------------------

instance HasLens (Maybe WitnessWebParams) WitnessContext (Maybe WitnessWebParams) where
    lensOf = wcParams . wpWebParamsL
instance HasLens MetricsEndpoint WitnessContext MetricsEndpoint where
    lensOf = wcParams . wpMetricsEndpointL
instance HasLens LoggingIO WitnessContext LoggingIO where
    lensOf = wcResources . wrLogging
instance HasLens RocksDB WitnessContext RocksDB where
    lensOf = wcResources . wrDB
instance HasLens Z.ZTGlobalEnv WitnessContext Z.ZTGlobalEnv where
    lensOf = wcResources . wrNetwork . lensOf @Z.ZTGlobalEnv
instance HasLens Z.ZTNetCliEnv WitnessContext Z.ZTNetCliEnv where
    lensOf = wcResources . wrNetwork . lensOf @Z.ZTNetCliEnv
instance HasLens Z.ZTNetServEnv WitnessContext Z.ZTNetServEnv where
    lensOf = wcResources . wrNetwork . lensOf @Z.ZTNetServEnv
instance HasLens (KeyResources WitnessNode) WitnessContext (KeyResources WitnessNode) where
    lensOf = wcResources . wrKey
instance HasLens MempoolVar WitnessContext MempoolVar where
    lensOf = wcMempool
instance HasLens SDVars WitnessContext SDVars where
    lensOf = wcSDActions
instance HasLens RelayState WitnessContext RelayState where
    lensOf = wcRelayState
-- instance HasLens SD.OSParamsBuilder WitnessContext SD.OSParamsBuilder where
--     lensOf = wcSDActions . nsSDParamsBuilder
instance HasLens SDLock WitnessContext SDLock where
    lensOf = wcSDLock

----------------------------------------------------------------------------
-- Sanity check
----------------------------------------------------------------------------

_sanity :: WitnessRealMode ()
_sanity = withWitnessConfig (error "") _sanityCallee
  where
    _sanityCallee :: FullWitnessWorkMode ctx m => m ()
    _sanityCallee = pass
