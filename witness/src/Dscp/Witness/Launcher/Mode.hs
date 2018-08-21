{-# LANGUAGE TemplateHaskell #-}

-- | Module contains the definition of Witness's WorkMode and its implementations.

module Dscp.Witness.Launcher.Mode
    (
      -- * Markers
      WitnessNode

      -- * Constraints
    , WitnessWorkMode

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

import Dscp.DB.Rocks.Class (MonadDB)
import Dscp.DB.Rocks.Real.Types (RocksDB)
import qualified Dscp.Launcher.Mode as Basic
import Dscp.Rio (RIO)
import Dscp.Network ()
import Dscp.Resource.Keys (KeyResources)
import Dscp.Snowdrop.Actions (SDActions)
import Dscp.Witness.Config (HasWitnessConfig, withWitnessConfig)
import Dscp.Witness.Launcher.Marker (WitnessNode)
import Dscp.Witness.Launcher.Params (WitnessParams)
import Dscp.Witness.Launcher.Resource (WitnessResources, wrDB, wrKey, wrLogging, wrNetwork)
import Dscp.Witness.Mempool.Type (MempoolVar)
import Dscp.Witness.Relay (RelayState)
import Dscp.Witness.SDLock (SDLock)

---------------------------------------------------------------------
-- WorkMode class
---------------------------------------------------------------------

-- | Set of typeclasses which define capabilities of Witness node.
type WitnessWorkMode ctx m =
    ( Basic.BasicWorkMode m
    , MonadDB m
    , NetworkingCli ZmqTcp m
    , NetworkingServ ZmqTcp m

    , HasWitnessConfig

    , MonadReader ctx m

    , HasLens' ctx WitnessParams
    , HasLens' ctx LoggingIO
    , HasLens' ctx RocksDB
    , HasLens' ctx Z.ZTGlobalEnv
    , HasLens' ctx Z.ZTNetCliEnv
    , HasLens' ctx Z.ZTNetServEnv
    , HasLens' ctx MempoolVar
    , HasLens' ctx SDActions
    , HasLens' ctx (KeyResources WitnessNode)
    , HasLens' ctx RelayState
    , HasLens' ctx SD.OSParamsBuilder
    , HasLens' ctx SDLock
    )

---------------------------------------------------------------------
-- WorkMode implementation
---------------------------------------------------------------------

-- | Context is resources plus some runtime variables.
data WitnessContext = WitnessContext
    { _wcParams          :: !WitnessParams     -- ^ Parameters witness was started with.
    , _wcResources       :: !WitnessResources  -- ^ Resources, allocated from params.
    , _wcMempool         :: !MempoolVar
    , _wcSDActions       :: !SDActions
    , _wcRelayState      :: !RelayState
    , _wcSDParamsBuilder :: !SD.OSParamsBuilder
    , _wcSDLock          :: !SDLock
    }


makeLenses ''WitnessContext

type WitnessRealMode = RIO WitnessContext

---------------------------------------------------------------------
-- HasLens
---------------------------------------------------------------------

instance HasLens WitnessParams WitnessContext WitnessParams where
    lensOf = wcParams
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
instance HasLens SDActions WitnessContext SDActions where
    lensOf = wcSDActions
instance HasLens RelayState WitnessContext RelayState where
    lensOf = wcRelayState
instance HasLens SD.OSParamsBuilder WitnessContext SD.OSParamsBuilder where
    lensOf = wcSDParamsBuilder
instance HasLens SDLock WitnessContext SDLock where
    lensOf = wcSDLock

----------------------------------------------------------------------------
-- Sanity check
----------------------------------------------------------------------------

_sanity :: WitnessRealMode ()
_sanity = withWitnessConfig (error "") _sanityCallee
  where
    _sanityCallee :: WitnessWorkMode ctx m => m ()
    _sanityCallee = pass
