module Dscp.Educator.Web.Params
    ( EducatorWebParams (..)
    ) where

import Dscp.Educator.Web.Bot.Params
import Dscp.Web

data EducatorWebParams = EducatorWebParams
    { ewpServerParams :: ServerParams
    , ewpBotParams    :: EducatorBotSwitch
    }
