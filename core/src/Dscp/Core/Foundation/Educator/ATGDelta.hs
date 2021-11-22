
module Dscp.Core.Foundation.Educator.ATGDelta where

import Dscp.Core.Foundation.Educator.Subject
import Dscp.Util

data ATGSubjectChange
    = ATGAdded
    | ATGRemoved
    deriving (Eq, Ord, Show, Generic)

instance Buildable ATGSubjectChange where
    build = \case
        ATGAdded -> "added"
        ATGRemoved -> "removed"

-- | ATGDelta is a diff for set of subjects which are taught by Educator.
-- Implemented as 'Map SubjectId Bool' to avoid representing invalid diffs
-- (like, subject is present simultaneously in added and removed sets).
-- TODO: maybe we should separately make up a library for such stuff,
-- like 'MapModifier'?
newtype ATGDelta = ATGDelta
    { getATGDelta :: Map (Id Subject) ATGSubjectChange
    } deriving (Show, Eq, Ord, Monoid, Generic)

instance Buildable ATGDelta where
    build (ATGDelta d) = "ATGDelta { " +| mapF d |+ " }"

-- | Whether does 'ATGDelta' carries no changes actually.
isEmptyATGDelta :: ATGDelta -> Bool
isEmptyATGDelta  = null . getATGDelta
