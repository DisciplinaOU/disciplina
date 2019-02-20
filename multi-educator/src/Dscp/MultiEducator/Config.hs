{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

-- | All educator's configurations.

module Dscp.MultiEducator.Config
    ( MultiEducatorWebConfig
    , MultiEducatorWebConfigRec
    , MultiEducatorConfig
    , MultiEducatorConfigRec
    , HasMultiEducatorConfig
    , defaultMultiEducatorConfig
    , multiEducatorConfig
    , withMultiEducatorConfig
    , fillMultiEducatorConfig

    , module Dscp.Witness.Config
    ) where

import Control.Lens ((?~))
import Data.Reflection (Given, give, given)
import Loot.Config ((:::), (::<), ConfigKind (Final, Partial), ConfigRec, upcast)
import Time (Second, Time)

import Dscp.Config
import Dscp.DB.SQL
import Dscp.Educator.Web.Auth
import Dscp.Educator.Web.Bot.Params
import Dscp.MultiEducator.Launcher.Params
import Dscp.Web
import Dscp.Witness.Config

type MultiEducatorWebConfig =
    '[ "serverParams"           ::< ServerParams
     , "botConfig"              ::< EducatorBotConfig
     , "multiEducatorAPINoAuth" ::: NoAuthContext "multi-educator"
     , "studentAPINoAuth"       ::: NoAuthContext "student"
     ]

type MultiEducatorWebConfigRec = ConfigRec 'Final MultiEducatorWebConfig

type MultiEducatorConfig = WitnessConfig ++
    '[ "educator" ::<
       '[ "db" ::< PostgresRealParams
        , "keys" ::: MultiEducatorKeyParams
        , "aaa" ::< MultiEducatorAAAConfig
        , "api" ::< MultiEducatorWebConfig
        , "publishing" ::<
           '[ "period" ::: Time Second
            ]
        , "certificates" ::<
           '[ "latex" ::: FilePath
            , "resources" ::: FilePath
            ]
        ]
     ]

type MultiEducatorConfigRecP = ConfigRec 'Partial MultiEducatorConfig
type MultiEducatorConfigRec = ConfigRec 'Final MultiEducatorConfig

type HasMultiEducatorConfig = Given MultiEducatorConfigRec

defaultMultiEducatorConfig :: MultiEducatorConfigRecP
defaultMultiEducatorConfig = upcast defaultWitnessConfig
    & sub #educator . sub #db .~ defaultPostgresRealParams
    & sub #educator . sub #api . sub #botConfig . tree #params . selection ?~ "disabled"
    & sub #educator . sub #certificates . option #latex ?~ "xelatex"

-- instance (HasEducatorConfig, cfg ~ WitnessConfigRec) => Given cfg where
--     given = rcast (given @EducatorConfigRec)

multiEducatorConfig :: HasMultiEducatorConfig => MultiEducatorConfigRec
multiEducatorConfig = given

withMultiEducatorConfig :: MultiEducatorConfigRec -> (HasMultiEducatorConfig => a) -> a
withMultiEducatorConfig = give

fillMultiEducatorConfig :: MultiEducatorConfigRecP -> IO MultiEducatorConfigRecP
fillMultiEducatorConfig = fillExpandedConfig fillWitnessConfig
