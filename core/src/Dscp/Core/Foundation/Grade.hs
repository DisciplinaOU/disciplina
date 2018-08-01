-- | Utils for working with grades

module Dscp.Core.Foundation.Grade
       ( gA
       , gB
       , gC
       , gD
       , gE
       , gF
       ) where

import Dscp.Core.Foundation.Educator (Grade (..))

gA, gB, gC, gD, gE, gF :: Grade
gA = UnsafeGrade 100
gB = UnsafeGrade 80
gC = UnsafeGrade 60
gD = UnsafeGrade 40
gE = UnsafeGrade 20
gF = UnsafeGrade 0
