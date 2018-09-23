module Dscp.Educator.Web.Params
    ( EducatorWebParams (..)
    ) where

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveFromJSON)

import Dscp.Educator.Web.Bot.Params
import Dscp.Web

data EducatorWebParams = EducatorWebParams
    { ewpServerParams :: ServerParams
    , ewpBotParams    :: EducatorBotSwitch
    }

deriveFromJSON defaultOptions ''EducatorWebParams
