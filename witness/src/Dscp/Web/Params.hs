-- | Utils for serving HTTP APIs

module Dscp.Web.Params
       ( ServerParams (..)
       ) where

import Dscp.Web.Types

-- | Params needed to start (servant) server.
data ServerParams = ServerParams
    { spAddr :: !NetworkAddress
    } deriving (Eq, Show, Generic)
