module Dscp.Educator.Launcher.Params
       ( EducatorKeyParams
       , EducatorKeyParamsRec
       , EducatorKeyParamsRecP
       ) where

import Loot.Config ((::<), Config, PartialConfig)

import Dscp.Resource.Keys (BaseKeyParams)

-- | Educator key parameters.
type EducatorKeyParams =
   '[ "keyParams" ::< BaseKeyParams
    ]

type EducatorKeyParamsRec = Config EducatorKeyParams
type EducatorKeyParamsRecP = PartialConfig EducatorKeyParams
