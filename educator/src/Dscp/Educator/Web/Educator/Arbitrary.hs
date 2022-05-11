-- | Generators and examples of educator-specific types.

module Dscp.Educator.Web.Educator.Arbitrary
    ( educatorCourseInfoEx
    , educatorSubmissionInfoEx
    , certificateListEx
    ) where

import Universum

import Dscp.Core
import Dscp.Crypto
import Dscp.Educator.Web.Arbitrary
import Dscp.Educator.Web.Educator.Types
import Dscp.Util.Test

educatorCourseInfoEx :: CourseEducatorInfo
educatorCourseInfoEx =
    CourseEducatorInfo
    { ciId = 0
    , ciDesc = "Patakology"
    , ciSubjects = []
    }

educatorSubmissionInfoEx :: SubmissionEducatorInfo
educatorSubmissionInfoEx =
    SubmissionEducatorInfo
    { siHash = hash submissionEx
    , siContentsHash = offlineHash
    , siAssignmentHash = hash assignmentEx
    , siGrade = Just gradeInfoEx
    , siWitness = submissionWitnessEx
    }

certificateListEx :: [Certificate]
certificateListEx = detGen 123 $ vectorOf 200 $ mkCertificate <$> arbitrary <*> arbitrary
