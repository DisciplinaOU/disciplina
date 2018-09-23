{-# LANGUAGE TypeOperators #-}

-- | All educator's configurations.

module Dscp.Educator.Config
    ( EducatorConfig
    , EducatorConfigRec
    , HasEducatorConfig
    , educatorConfig
    , withEducatorConfig
    , fillEducatorConfig

    , module Dscp.Witness.Config
    ) where

import Data.Reflection (Given, give, given)
import Loot.Config ((:::), (::<), ConfigKind (Final, Partial), ConfigRec)

import Dscp.Config
import Dscp.DB.SQLite (SQLiteParams)
import Dscp.Educator.Launcher.Params (EducatorKeyParams)
import Dscp.Educator.Web.Params (EducatorWebParams)
import Dscp.Witness.Config

type EducatorConfig = WitnessConfig ++
    '[ "educator" ::<
       '[ "db" ::: SQLiteParams
        , "keys" ::: EducatorKeyParams
        , "api" ::: EducatorWebParams
        ]
     ]

type EducatorConfigRecP = ConfigRec 'Partial EducatorConfig
type EducatorConfigRec = ConfigRec 'Final EducatorConfig

type HasEducatorConfig = Given EducatorConfigRec

-- instance (HasEducatorConfig, cfg ~ WitnessConfigRec) => Given cfg where
--     given = rcast (given @EducatorConfigRec)

educatorConfig :: HasEducatorConfig => EducatorConfigRec
educatorConfig = given

withEducatorConfig :: EducatorConfigRec -> (HasEducatorConfig => a) -> a
withEducatorConfig = give

fillEducatorConfig :: EducatorConfigRecP -> IO EducatorConfigRecP
fillEducatorConfig = fillExpandedConfig fillWitnessConfig
