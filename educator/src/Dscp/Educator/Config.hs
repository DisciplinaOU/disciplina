{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

-- | All educator's configurations.

module Dscp.Educator.Config
    ( EducatorConfig
    , EducatorConfigRec
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
import Dscp.DB.SQLite
import Dscp.Educator.Launcher.Params
import Dscp.Educator.Web.Bot.Params
import Dscp.Educator.Web.Config
import Dscp.Resource.Keys
import Dscp.Witness.Config

type EducatorConfig = WitnessConfig ++
    '[ "educator" ::<
       '[ "db" ::: SQLiteParams
        , "keys" ::: EducatorKeyParams
        , "api" ::< EducatorWebConfig
        , "publishing" ::<
           '[ "period" ::: Time Second
            ]
        ]
     ]

type EducatorConfigRecP = ConfigRec 'Partial EducatorConfig
type EducatorConfigRec = ConfigRec 'Final EducatorConfig

type HasEducatorConfig = Given EducatorConfigRec

defaultEducatorConfig :: EducatorConfigRecP
defaultEducatorConfig = upcast defaultWitnessConfig
    & sub #educator . option #db ?~ defSqliteParams
    & sub #educator . option #keys ?~ defKeyParams
    & sub #educator . sub #api . option #botParams ?~ defBotParams
  where
    defSqliteParams = SQLiteParams $ SQLiteReal $ SQLiteRealParams
        { srpPath = "educator-db"
        , srpConnNum = Nothing
        , srpMaxPending = 200
        }
    defKeyParams = EducatorKeyParams $ BaseKeyParams Nothing False Nothing
    defBotParams = EducatorBotParams False "Memes generator" 0

-- instance (HasEducatorConfig, cfg ~ WitnessConfigRec) => Given cfg where
--     given = rcast (given @EducatorConfigRec)

educatorConfig :: HasEducatorConfig => EducatorConfigRec
educatorConfig = given

withEducatorConfig :: EducatorConfigRec -> (HasEducatorConfig => a) -> a
withEducatorConfig = give

fillEducatorConfig :: EducatorConfigRecP -> IO EducatorConfigRecP
fillEducatorConfig = fillExpandedConfig fillWitnessConfig
