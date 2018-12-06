module Dscp.Educator.TestConfig
    ( testEducatorConfig
    ) where

import Data.Default (def)

import Dscp.Config.Util
import Dscp.Educator.Config
import Dscp.Witness.TestConfig

testEducatorConfigP :: EducatorConfigRecP
testEducatorConfigP = rreplace testWitnessConfigP def

testEducatorConfig :: EducatorConfigRec
testEducatorConfig = finaliseDeferredUnsafe testEducatorConfigP
