-- | Resources used by Witness node

module Dscp.Witness.Launcher.Resource
       ( WitnessResources (..)
       , wrLogging
       , wrDB
       , wrNetwork
       , wrKey
       ) where

import Loot.Log.Internal (NameSelector (GivenName), logNameSelL)
import Loot.Log.Rio (LoggingIO)

import Control.Lens (makeLenses)
import Loot.Base.HasLens (HasLens (..))
import Loot.Network.ZMQ (ZTGlobalEnv, ZTNetCliEnv, ZTNetServEnv)

import Dscp.DB.Rocks.Real (RocksDB)
import Dscp.Resource.Class (AllocResource (..), buildComponentR)
import Dscp.Resource.Keys (KeyResources (..), linkStore)
import Dscp.Resource.Network (NetLogging (..), NetServResources, withNetLogging)
import Dscp.Resource.Rocks ()
import Dscp.Witness.Config (HasWitnessConfig)
import Dscp.Witness.Launcher.Marker (WitnessNode)
import Dscp.Witness.Launcher.Params (WitnessKeyParams (..), WitnessParams (..))

-- | Datatype which contains resources required by witness node to start
-- working.
data WitnessResources = WitnessResources
    { _wrLogging :: !LoggingIO
    , _wrDB      :: !RocksDB
    , _wrNetwork :: !NetServResources
    , _wrKey     :: !(KeyResources WitnessNode)
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

instance HasWitnessConfig =>
         AllocResource WitnessKeyParams (KeyResources WitnessNode) where
    allocResource WitnessKeyParams{..} =
        buildComponentR "witness keys"
            (linkStore wkpBase wkpCommittee)
            (\_ -> pass)

instance HasWitnessConfig => AllocResource WitnessParams WitnessResources where
    allocResource WitnessParams{..} = do
        _wrLogging <- view (lensOf @LoggingIO)
        _wrDB <- allocResource wpDBParams
        _wrNetwork <- do
            let modGivenName (GivenName x) = GivenName $ x <> "network"
                modGivenName x             = x
            withNetLogging (NetLogging $ _wrLogging & logNameSelL %~ modGivenName)
                           (allocResource wpNetworkParams)
        _wrKey <- allocResource wpKeyParams
        return WitnessResources {..}
