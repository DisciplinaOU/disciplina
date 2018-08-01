module Dscp.Educator.Web.Params
    ( EducatorWebParams (..)
    ) where

import Dscp.Web

data EducatorWebParams = EducatorWebParams
    { ewpServerParams :: ServerParams
    , ewpWithBot      :: Bool
    }
