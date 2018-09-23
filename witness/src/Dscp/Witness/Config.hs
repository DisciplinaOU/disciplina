{-# LANGUAGE TypeOperators #-}

-- | All witness's configurations.

module Dscp.Witness.Config
    ( WitnessConfig
    , WitnessConfigRec
    , HasWitnessConfig
    , witnessConfig
    , withWitnessConfig
    , fillWitnessConfig

    , module Dscp.Core.Config
    ) where

import Data.Reflection (Given (..), give)
import Loot.Config ((:::), (::<), ConfigKind (Final, Partial), ConfigRec)

import Dscp.Config
import Dscp.Core.Config
import Dscp.DB.Rocks.Real.Types (RocksDBParams)
import Dscp.Resource.AppDir (AppDirParam)
import Dscp.Resource.Logging (LoggingParams)
import Dscp.Resource.Network (NetServParams)
import Dscp.Web (MetricsEndpoint, ServerParams)
import Dscp.Witness.Launcher.Params (WitnessKeyParams)

type WitnessConfig = CoreConfig ++
    '[ "witness" ::<
       '[ "logging" ::: LoggingParams
        , "db" ::: RocksDBParams
        , "network" ::: NetServParams
        , "keys" ::: WitnessKeyParams
        , "api" ::: Maybe ServerParams
        , "appDir" ::: AppDirParam
        , "metricsEndpoint" ::: MetricsEndpoint
        ]
     ]

type WitnessConfigRecP = ConfigRec 'Partial WitnessConfig
type WitnessConfigRec = ConfigRec 'Final WitnessConfig

type HasWitnessConfig = Given WitnessConfigRec

-- | Instance for making sure that 'HasCoreConfig' is derivable
-- from 'HasWitnessConfig'
instance HasWitnessConfig => Given CoreConfigRec where
    given = rcast (given @WitnessConfigRec)

---------------------------------------------------------------------------
-- Config itself
---------------------------------------------------------------------------

witnessConfig :: HasWitnessConfig => WitnessConfigRec
witnessConfig = given

withWitnessConfig :: WitnessConfigRec -> (HasWitnessConfig => a) -> a
withWitnessConfig = give

fillWitnessConfig :: WitnessConfigRecP -> IO WitnessConfigRecP
fillWitnessConfig = fillExpandedConfig fillCoreConfig

