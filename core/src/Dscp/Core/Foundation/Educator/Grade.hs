
module Dscp.Core.Foundation.Educator.Grade where

import Dscp.Util

-- | Assignment/course grade.
-- An integer from 0 to 100. Constructor is unsafe, because it's possible
-- to make a grade outside these bounds.
newtype Grade = UnsafeGrade
    { getGrade :: Word8
    } deriving (Eq, Ord, Show, Generic)

instance Bounded Grade where
    minBound = UnsafeGrade 0
    maxBound = UnsafeGrade 100

instance Buildable Grade where
    build = build . getGrade

mkGrade :: Word8 -> Maybe Grade
mkGrade a =
    let g = UnsafeGrade a
    in  g <$ guard (g >= minBound && g <= maxBound)
