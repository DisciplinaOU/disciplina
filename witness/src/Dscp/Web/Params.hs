-- | Utils for serving HTTP APIs

module Dscp.Web.Params
       ( ServerParams
       , ServerParamsRec
       , ServerParamsRecP
       ) where

import Loot.Config ((:::), Config, PartialConfig)

import Dscp.Web.Types

-- | Params needed to start (servant) server.
type ServerParams =
   '[ "addr" ::: NetworkAddress
    ]

type ServerParamsRec = Config ServerParams
type ServerParamsRecP = PartialConfig ServerParams
