-- | Utils for working with grades

module Dscp.Core.Foundation.Grade
       ( gA
       , gB
       , gC
       , gD
       , gE
       , gF

       , isPositiveGrade
       ) where

import Universum
import Dscp.Core.Foundation.Educator (Grade (..))

gA, gB, gC, gD, gE, gF :: Grade
gA = UnsafeGrade 100
gB = UnsafeGrade 80
gC = UnsafeGrade 60
gD = UnsafeGrade 40
gE = UnsafeGrade 20
gF = UnsafeGrade 0

-- | Whether assignment with this grade should be considered passed.
-- NOTE: there is a "isPositiveGradeQ" function which *must* work in the same way.
isPositiveGrade :: Grade -> Bool
isPositiveGrade _ = True  -- most easy scenario to work with, fine for alpha ver
