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
    ) where

import Universum

import Control.Lens ((?~))
import Data.Reflection (Given, give, given)
import Loot.Config (ConfigKind (Final, Partial), ConfigRec, (:::), (::<))
import Time (Second, Time)

import Dscp.Config
import Dscp.Core.Foundation.Educator (Language (..))
import Dscp.DB.SQL
import Dscp.Educator.Web.Auth
import Dscp.MultiEducator.Launcher.Params
import Dscp.Resource.AppDir (AppDirParam)
import Dscp.Resource.Logging (LoggingParams, basicLoggingParams)
import Dscp.Web


type MultiEducatorWebConfig =
    '[ "serverParams"           ::< ServerParams
     , "multiEducatorAPINoAuth" ::: NoAuthContext "multi-educator"
     , "studentAPINoAuth"       ::: NoAuthContext "student"
     ]

type MultiEducatorWebConfigRecP = ConfigRec 'Partial MultiEducatorWebConfig
type MultiEducatorWebConfigRec = ConfigRec 'Final MultiEducatorWebConfig

type MultiEducatorConfig =
    '[ "educator" ::<
       '[ "logging" ::< LoggingParams
        , "db" ::< PostgresRealParams
        , "appDir" ::< AppDirParam
        , "keys" ::: MultiEducatorKeyParams
        , "aaa" ::< MultiEducatorAAAConfig
        , "api" ::< MultiEducatorWebConfig
        , "publishing" ::<
           '[ "period" ::: Time Second
            ]
        , "certificates" ::<
           '[ "language" ::: Language
            , "latex" ::: FilePath
            , "resources" ::: FilePath
            , "downloadBaseUrl" ::: BaseUrl
            ]
        ]
     ]

type MultiEducatorConfigRecP = ConfigRec 'Partial MultiEducatorConfig
type MultiEducatorConfigRec = ConfigRec 'Final MultiEducatorConfig

type HasMultiEducatorConfig = Given MultiEducatorConfigRec

defaultMultiEducatorConfig :: MultiEducatorConfigRecP
defaultMultiEducatorConfig = mempty
    & sub #educator . sub #logging .~ basicLoggingParams "educator" False
    & sub #educator . sub #db .~ defaultPostgresRealParams
    & sub #educator . sub #appDir . tree #param . selection ?~ "os"
    & sub #educator . sub #api .~ defaultMultiEducatorWebConfig
    & sub #educator . sub #certificates . option #language ?~ EN
    & sub #educator . sub #certificates . option #latex ?~ "xelatex"

defaultMultiEducatorWebConfig :: MultiEducatorWebConfigRecP
defaultMultiEducatorWebConfig = mempty
    & option #multiEducatorAPINoAuth ?~ NoAuthOffContext
    & option #studentAPINoAuth ?~ NoAuthOffContext

-- instance (HasEducatorConfig, cfg ~ WitnessConfigRec) => Given cfg where
--     given = rcast (given @EducatorConfigRec)

multiEducatorConfig :: HasMultiEducatorConfig => MultiEducatorConfigRec
multiEducatorConfig = given

withMultiEducatorConfig :: MultiEducatorConfigRec -> (HasMultiEducatorConfig => a) -> a
withMultiEducatorConfig = give

fillMultiEducatorConfig :: MultiEducatorConfigRecP -> IO MultiEducatorConfigRecP
fillMultiEducatorConfig = return
