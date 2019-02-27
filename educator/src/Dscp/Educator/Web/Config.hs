{-# LANGUAGE TypeOperators #-}

module Dscp.Educator.Web.Config
    ( EducatorWebConfig
    , EducatorWebConfigRecP
    , EducatorWebConfigRec
    ) where

import Loot.Config ((:::), (::<), ConfigKind (Final, Partial), ConfigRec)

import Dscp.Educator.Web.Bot.Params
import Dscp.Educator.Web.Educator.Auth ()
import Dscp.Educator.Web.Student.Auth ()
import Dscp.Util.Servant.Auth
import Dscp.Web

type EducatorWebConfig =
    '[ "serverParams"      ::< ServerParams
     , "botConfig"         ::< EducatorBotConfig
     , "educatorAPINoAuth" ::: NoAuthContext "educator"
     , "studentAPINoAuth"  ::: NoAuthContext "student"
     , "authTimeout"       ::: AuthTimeout
     ]

type EducatorWebConfigRecP = ConfigRec 'Partial EducatorWebConfig
type EducatorWebConfigRec = ConfigRec 'Final EducatorWebConfig
