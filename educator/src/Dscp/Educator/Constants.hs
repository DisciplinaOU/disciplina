module Dscp.Educator.Constants
    ( defCertCourse
    , defCertStudentSk
    , defCertStudent
    , defCertAssignment
    ) where

import Dscp.Core
import Dscp.Crypto

defCertCourse :: Course
defCertCourse = Course (-1)

defCertStudentSk :: SecretKeyData
defCertStudentSk = mkSecretKeyData $ withIntSeed 789 genSecretKey

-- | Student address used whenever the actual address is unknown to educator
-- when creating a certificate (in particular, student may have no address at all).
defCertStudent :: Student
defCertStudent = skAddress defCertStudentSk

-- | Assignment refered by every certificate grade by default.
defCertAssignment :: Assignment
defCertAssignment =
    Assignment
    { _aCourseId = defCertCourse
    , _aContentsHash = seedHash "Jump into a black hole"
    , _aType = CourseFinal
    , _aDesc = ""
    }

-- TODO: Someday we will need to add description for each field of certificate datatypes
-- for the sake of swagger documentation, mention those things ^ as example there.
