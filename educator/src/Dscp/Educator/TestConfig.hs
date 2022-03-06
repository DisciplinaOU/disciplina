module Dscp.Educator.TestConfig
    ( testEducatorConfig
    ) where

import Data.Default (def)

import Dscp.Config.Util
import Dscp.Educator.Config

testEducatorConfigP :: EducatorConfigRecP
testEducatorConfigP = def

testEducatorConfig :: EducatorConfigRec
testEducatorConfig = finaliseDeferredUnsafe testEducatorConfigP
