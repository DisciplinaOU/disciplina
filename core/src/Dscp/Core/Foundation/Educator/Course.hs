
module Dscp.Core.Foundation.Educator.Course where

import Dscp.Util

-- | Educator's course ID is simply a number too.
-- There's a mapping from course ID to a set of associated subject IDs.
newtype Course = Course
    { getCourseId :: Int64
    } deriving (Eq, Ord, Show, Num)

instance HasId Course

instance Buildable Course where
    build Course{..} = build getCourseId
