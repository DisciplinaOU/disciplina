module Dscp.Educator.Web.Params
    ( EducatorWebParams (..)
    ) where

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveFromJSON)

import Dscp.Educator.Web.Auth
import Dscp.Educator.Web.Bot.Params
import Dscp.Educator.Web.Educator.Auth ()
import Dscp.Educator.Web.Student.Auth ()
import Dscp.Web

data EducatorWebParams = EducatorWebParams
    { ewpServerParams      :: ServerParams
    , ewpBotParams         :: EducatorBotSwitch
    , ewpEducatorAPINoAuth :: NoAuthContext "educator"
    , ewpStudentAPINoAuth  :: NoAuthContext "student"
    } deriving (Show, Eq)

deriveFromJSON defaultOptions ''EducatorWebParams
