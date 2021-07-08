
module Dscp.Core.Foundation.Educator.GradeInfo where

import Dscp.Core.Foundation.Educator.Grade
import Dscp.Core.Foundation.Educator.Submission
import Dscp.Core.Foundation.Educator.Timestamp
import Dscp.Crypto
import Dscp.Util

data GradeInfo = GradeInfo
    { giSubmissionHash :: (Hash Submission)
    , giGrade          :: Grade
    , giTimestamp      :: Timestamp
    , giHasProof       :: Bool
    } deriving (Show, Eq, Ord, Generic)

instance Buildable (GradeInfo) where
    build (GradeInfo{..}) =
      "{ submission hash = " +| giSubmissionHash |+
      ", grade = " +| giGrade |+
      ", timestamp = " +| giTimestamp |+
      ", has proof = " +| giHasProof |+
      " }"
