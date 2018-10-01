{-# LANGUAGE TypeOperators #-}

-- | All educator's configurations.

module Dscp.MultiEducator.Config
    ( MultiEducatorConfig
    , MultiEducatorConfigRec
    , HasMultiEducatorConfig
    , multiEducatorConfig
    , withMultiEducatorConfig
    , fillMultiEducatorConfig

    , module Dscp.Witness.Config
    ) where

import Data.Reflection (Given, give, given)
import Loot.Config ((:::), (::<), ConfigKind (Final, Partial), ConfigRec)
import Time (Second, Time)

import Dscp.Config
import Dscp.DB.SQLite (SQLiteParams)
import Dscp.MultiEducator.Launcher.Params (MultiEducatorKeyParams)
import Dscp.Educator.Web.Params (EducatorWebParams)
import Dscp.Witness.Config

type MultiEducatorConfig = WitnessConfig ++
    '[ "educator" ::<
       '[ "db" ::: SQLiteParams
        , "keys" ::: MultiEducatorKeyParams
        , "api" ::: EducatorWebParams
        , "publishing" ::<
           '[ "period" ::: Time Second
            ]
        ]
     ]

type MultiEducatorConfigRecP = ConfigRec 'Partial MultiEducatorConfig
type MultiEducatorConfigRec = ConfigRec 'Final MultiEducatorConfig

type HasMultiEducatorConfig = Given MultiEducatorConfigRec

-- instance (HasEducatorConfig, cfg ~ WitnessConfigRec) => Given cfg where
--     given = rcast (given @EducatorConfigRec)

multiEducatorConfig :: HasMultiEducatorConfig => MultiEducatorConfigRec
multiEducatorConfig = given

withMultiEducatorConfig :: MultiEducatorConfigRec -> (HasMultiEducatorConfig => a) -> a
withMultiEducatorConfig = give

fillMultiEducatorConfig :: MultiEducatorConfigRecP -> IO MultiEducatorConfigRecP
fillMultiEducatorConfig = fillExpandedConfig fillWitnessConfig
