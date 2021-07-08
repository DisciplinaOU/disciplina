
module Dscp.Core.Foundation.Educator.Subject where

import Dscp.Util

-- TODO [DSCP-416]: extract "Int64" to reasonable type helper.
-- | ID of particular subject.
newtype Subject = Subject
    { getSubjectId :: Int64
    } deriving (Eq, Ord, Show, Num)

instance Buildable Subject where
    build (Subject n) = build n

instance HasId Subject
