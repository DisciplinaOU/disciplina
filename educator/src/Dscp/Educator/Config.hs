{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

-- | All educator's configurations.

module Dscp.Educator.Config
    ( EducatorConfig
    , EducatorConfigRec
    , EducatorConfigRecP
    , HasEducatorConfig
    , defaultEducatorConfig
    , educatorConfig
    , withEducatorConfig
    , fillEducatorConfig

    , module Dscp.Witness.Config
    ) where

import Control.Lens ((?~))
import Data.Reflection (Given, give, given)
import Loot.Config ((:::), (::<), ConfigKind (Final, Partial), ConfigRec, upcast)
import Time (Second, Time)

import Dscp.Config
import Dscp.DB.SQL
import Dscp.Educator.Launcher.Params
import Dscp.Educator.Web.Config
import Dscp.Resource.Keys
import Dscp.Witness.Config

type EducatorConfig = WitnessConfig ++
    '[ "educator" ::<
       '[ "db" ::< PostgresRealParams
        , "keys" ::< EducatorKeyParams
        , "api" ::< EducatorWebConfig
        , "publishing" ::<
           '[ "period" ::: Time Second
            ]
        , "certificates" ::<
           '[ "resources" ::: FilePath
            ]
        ]
     ]

type EducatorConfigRecP = ConfigRec 'Partial EducatorConfig
type EducatorConfigRec = ConfigRec 'Final EducatorConfig

type HasEducatorConfig = (Given EducatorConfigRec, HasWitnessConfig)

defaultEducatorConfig :: EducatorConfigRecP
defaultEducatorConfig = upcast defaultWitnessConfig
    & sub #educator . sub #db .~ defaultPostgresRealParams
    & sub #educator . sub #keys . sub #keyParams .~ defaultBaseKeyParams
    & sub #educator . sub #api . sub #botConfig . tree #params . selection ?~ "disabled"

-- instance (HasEducatorConfig, cfg ~ WitnessConfigRec) => Given cfg where
--     given = rcast (given @EducatorConfigRec)

educatorConfig :: HasEducatorConfig => EducatorConfigRec
educatorConfig = given

withEducatorConfig :: EducatorConfigRec -> (HasEducatorConfig => a) -> a
withEducatorConfig conf a = give (rcast @_ @WitnessConfig conf) $ give conf a

fillEducatorConfig :: EducatorConfigRecP -> IO EducatorConfigRecP
fillEducatorConfig = fillExpandedConfig fillWitnessConfig
