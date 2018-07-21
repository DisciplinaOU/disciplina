-- | Resources used by Witness node

module Dscp.Witness.Launcher.Resource
       ( WitnessResources (..)
       , wrLogging
       , wrDB
       , wrNetwork
       , wrKey
       ) where

import Loot.Log.Internal (logNameSelL, _GivenName)
import Loot.Log.Rio (LoggingIO)

import Control.Lens (makeLenses)
import Loot.Base.HasLens (HasLens (..))
import Loot.Network.ZMQ (ZTGlobalEnv, ZTNetCliEnv, ZTNetServEnv)

import Dscp.DB.Rocks.Real (RocksDB)
import Dscp.Resource (AllocResource (..), KeyResources (..), NetLogging (..), NetServResources,
                      withNetLogging)
import Dscp.Witness.Launcher.Params (WitnessParams (..))

-- | Datatype which contains resources required by witness node to start
-- working.
data WitnessResources = WitnessResources
    { _wrLogging :: !LoggingIO
    , _wrDB      :: !RocksDB
    , _wrNetwork :: !NetServResources
    , _wrKey     :: !KeyResources
    }

makeLenses ''WitnessResources

instance HasLens LoggingIO WitnessResources LoggingIO where
    lensOf = wrLogging
instance HasLens RocksDB WitnessResources RocksDB where
    lensOf = wrDB
instance HasLens ZTGlobalEnv WitnessResources ZTGlobalEnv where
    lensOf = wrNetwork . lensOf @ZTGlobalEnv
instance HasLens ZTNetCliEnv WitnessResources ZTNetCliEnv where
    lensOf = wrNetwork . lensOf @ZTNetCliEnv
instance HasLens ZTNetServEnv WitnessResources ZTNetServEnv where
    lensOf = wrNetwork . lensOf @ZTNetServEnv

instance AllocResource WitnessParams WitnessResources where
    allocResource WitnessParams{..} = do
        _wrLogging <- view (lensOf @LoggingIO)
        _wrDB <- allocResource wpDBParams
        _wrNetwork <-
            withNetLogging (NetLogging $ _wrLogging & logNameSelL . _GivenName %~ (<> "network")) $
            allocResource wpNetworkParams
        _wrKey <- allocResource wpKeyParams
        return WitnessResources {..}
