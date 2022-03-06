module Dscp.Educator.Launcher.Params
       ( EducatorKeyParams
       , EducatorKeyParamsRec
       , EducatorKeyParamsRecP
       , defaultBaseKeyParams
       ) where

import Loot.Config (Config, PartialConfig, (::<))

import Dscp.Educator.Resource (BaseKeyParams, defaultBaseKeyParams)

-- | Educator key parameters.
type EducatorKeyParams =
   '[ "keyParams" ::< BaseKeyParams
    ]

type EducatorKeyParamsRec = Config EducatorKeyParams
type EducatorKeyParamsRecP = PartialConfig EducatorKeyParams
