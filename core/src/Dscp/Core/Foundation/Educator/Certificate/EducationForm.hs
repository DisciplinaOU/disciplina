
module Dscp.Core.Foundation.Educator.Certificate.EducationForm where

import Dscp.Util

data EducationForm = Fulltime | Parttime | Fullpart
    deriving (Show, Eq, Generic, Enum, Bounded)

instance Buildable EducationForm where
    build = show
