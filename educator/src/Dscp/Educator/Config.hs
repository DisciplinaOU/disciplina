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
    ) where

import Control.Lens ((?~))
import Data.Reflection (Given, give, given)
import Loot.Config (ConfigKind (Final, Partial), ConfigRec, (:::), (::<))
import Time (Second, Time)
import Universum

import Dscp.Config
import Dscp.Core.Foundation.Educator (ItemDesc (..), Language (..))
import Dscp.Resource.Logging (LoggingParams, basicLoggingParams)
import Dscp.Core.Web
import Dscp.DB.SQL
import Dscp.Educator.Launcher.Params
import Dscp.Educator.Web.Config
import Dscp.Resource.AppDir (AppDirParam)
import Dscp.Util

type EducatorConfig =
    '[ "educator" ::<
       '[ "logging" ::< LoggingParams
        , "db" ::< PostgresRealParams
        , "appDir" ::< AppDirParam
        , "keys" ::< EducatorKeyParams
        , "api" ::< EducatorWebConfig
        , "publishing" ::<
           '[ "period" ::: Time Second
            ]
        , "certificates" ::<
           '[ "language" ::: Language
            , "latex" ::: FilePath
            , "resources" ::: FilePath
            , "downloadBaseUrl" ::: BaseUrl
            , "issuer" ::<
               '[ "name" ::: ItemDesc
                , "website" ::: ItemDesc
                , "id" ::: Text
                ]
            ]
        ]
     ]

type EducatorConfigRecP = ConfigRec 'Partial EducatorConfig
type EducatorConfigRec = ConfigRec 'Final EducatorConfig

type HasEducatorConfig = Given EducatorConfigRec

defaultEducatorConfig :: EducatorConfigRecP
defaultEducatorConfig = mempty
    & sub #educator . sub #logging .~ basicLoggingParams "educator" False
    & sub #educator . sub #db .~ defaultPostgresRealParams
    & sub #educator . sub #appDir . tree #param . selection ?~ "os"
    & sub #educator . sub #keys . sub #keyParams .~ defaultBaseKeyParams
    & sub #educator . sub #api . sub #botConfig . tree #params . selection ?~ "disabled"
    & sub #educator . sub #certificates . option #latex ?~ "xelatex"
    & sub #educator . sub #certificates . option #downloadBaseUrl ?~ defBaseUrl
    & sub #educator . sub #certificates . sub #issuer . option #id ?~ "Principal"
  where
    defBaseUrl = nothingToPanic "url is correct" $
                 parseBaseUrl "https://localhost/api/certificates/v1/cert"

-- instance (HasEducatorConfig, cfg ~ WitnessConfigRec) => Given cfg where
--     given = rcast (given @EducatorConfigRec)

educatorConfig :: HasEducatorConfig => EducatorConfigRec
educatorConfig = given

withEducatorConfig :: EducatorConfigRec -> (HasEducatorConfig => a) -> a
withEducatorConfig = give

fillEducatorConfig :: EducatorConfigRecP -> IO EducatorConfigRecP
fillEducatorConfig = return
-- fillEducatorConfig = fillExpandedConfig fillCoreConfig
