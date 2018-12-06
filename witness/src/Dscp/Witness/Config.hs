{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

-- | All witness's configurations.

module Dscp.Witness.Config
    ( WitnessConfig
    , WitnessConfigRec
    , WitnessConfigRecP
    , HasWitnessConfig
    , defaultWitnessConfig
    , witnessConfig
    , withWitnessConfig
    , fillWitnessConfig

    , module Dscp.Core.Config
    ) where

import Control.Lens ((?~))
import Data.Reflection (Given (..), give)
import Loot.Config ((:::), (::<), ConfigKind (Final, Partial), ConfigRec)

import Dscp.Config
import Dscp.Core.Config
import Dscp.DB.Rocks.Real.Types
import Dscp.Resource.AppDir (AppDirParam)
import Dscp.Resource.Keys.Types (defaultBaseKeyParams)
import Dscp.Resource.Logging (LoggingParams, basicLoggingParams)
import Dscp.Resource.Network (NetServParams)
import Dscp.Web (MetricsEndpoint (..), ServerParams)
import Dscp.Witness.Keys

type WitnessConfig = CoreConfig ++
    '[ "witness" ::<
       '[ "logging" ::< LoggingParams
        , "db" ::< RocksDBParams
        , "network" ::< NetServParams
        , "keys" ::< WitnessKeyParams
        , "api" ::< ConfigMaybe ServerParams
        , "appDir" ::< AppDirParam
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

defaultWitnessConfig :: WitnessConfigRecP
defaultWitnessConfig = mempty
    & sub #witness . sub #logging .~ basicLoggingParams "witness" False
    & sub #witness . sub #db .~ rocksDBParams
    & sub #witness . sub #keys .~ keysParams
    & sub #witness . sub #api . tree #maybe . selection ?~ "nothing"
    & sub #witness . sub #appDir . tree #param . selection ?~ "os"
    & sub #witness . option #metricsEndpoint ?~ MetricsEndpoint Nothing
  where
    keysParams :: WitnessKeyParamsRecP
    keysParams = mempty
        & tree #params . selection ?~ "basic"
        & tree #params . branch #basic .~ defaultBaseKeyParams
    rocksDBParams :: RocksDBParamsRecP
    rocksDBParams = mempty
        & option #path  ?~ "witness-db"
        & option #clean ?~ False

witnessConfig :: HasWitnessConfig => WitnessConfigRec
witnessConfig = given

withWitnessConfig :: WitnessConfigRec -> (HasWitnessConfig => a) -> a
withWitnessConfig = give

fillWitnessConfig :: WitnessConfigRecP -> IO WitnessConfigRecP
fillWitnessConfig = fillExpandedConfig fillCoreConfig
