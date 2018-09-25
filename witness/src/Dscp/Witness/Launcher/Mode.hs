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
    , WitnessVariables (..)
    , wcResources

      -- * RealMode
    , WitnessRealMode

    , -- to avoid importing snowdrop imn Educator
      SD.OSParamsBuilder
    ) where

import Control.Lens (makeLenses)

import Loot.Base.HasLens (HasLens')
import Loot.Log.Rio (LoggingIO)
import Loot.Network.Class (NetworkingCli, NetworkingServ)
import Loot.Network.ZMQ as Z
import qualified Snowdrop.Block as SD

import qualified Dscp.Launcher.Mode as Basic
import Dscp.Network ()
import Dscp.Resource.Keys (KeyResources)
import Dscp.Resource.Network
import Dscp.Rio (RIO)
import Dscp.Snowdrop.Actions (SDVars)
import Dscp.Util.HasLens
import Dscp.Witness.Config (HasWitnessConfig, withWitnessConfig)
import Dscp.Witness.Launcher.Marker (WitnessNode)
import Dscp.Witness.Launcher.Resource
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

data WitnessVariables = WitnessVariables
    { _wvMempool    :: !MempoolVar
    , _wvSDActions  :: !SDVars
    , _wvRelayState :: !RelayState
    , _wvSDLock     :: !SDLock
    }

makeLenses ''WitnessVariables
deriveHasLensDirect ''WitnessVariables

-- | Context is resources plus some runtime variables.
data WitnessContext = WitnessContext
    { _wcResources  :: !WitnessResources  -- ^ Resources, allocated from params.
    , _wcMempool    :: !MempoolVar
    , _wcSDActions  :: !SDVars
    , _wcRelayState :: !RelayState
    , _wcSDLock     :: !SDLock
    }

makeLenses ''WitnessContext
deriveHasLensDirect ''WitnessContext

type WitnessRealMode = RIO WitnessContext

---------------------------------------------------------------------
-- HasLens
---------------------------------------------------------------------

deriveHasLens 'wcResources ''WitnessContext ''WitnessResources
deriveHasLens 'wcResources ''WitnessContext ''NetServResources

----------------------------------------------------------------------------
-- Sanity check
----------------------------------------------------------------------------

_sanity :: WitnessRealMode ()
_sanity = withWitnessConfig (error "") _sanityCallee
  where
    _sanityCallee :: FullWitnessWorkMode ctx m => m ()
    _sanityCallee = pass
