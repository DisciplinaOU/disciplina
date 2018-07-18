module Dscp.Educator.Web.Params
       ( EducatorWebParams (..)
       ) where

import Dscp.Web (NetworkAddress)

-- | Contains all the data relevant to web APIs served by
-- Educator node.
data EducatorWebParams = EducatorWebParams
    { ewpStudentApiAddr :: !NetworkAddress
    } deriving (Eq, Show, Generic)
