module Test.Dscp.Educator.Web.Instances
    ( genCourseNoSubjects
    ) where

import Universum

import Dscp.Educator.DB
import Dscp.Educator.Web.Educator
import Dscp.Educator.Web.Student
import Dscp.Educator.Web.Types
import Dscp.Util.Test

instance Arbitrary CourseDetails where
    arbitrary = CourseDetails <$> (Just <$> arbitrary) <*> arbitrary <*> listUnique

genCourseNoSubjects :: Gen CourseDetails
genCourseNoSubjects = CourseDetails <$> (Just <$> arbitrary) <*> arbitrary <*> pure []

deriving instance Arbitrary IsFinal

instance Arbitrary StudentGetAssignmentsFilters where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary StudentGetSubmissionsFilters where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary EducatorGetAssignmentsFilters where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary EducatorGetSubmissionsFilters where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary GetProvenStudentTransactionsFilters where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary CourseStudentInfo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary AssignmentStudentInfo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary SubmissionStudentInfo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary GradeInfo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary BlkProofInfo where
    arbitrary = genericArbitrary
    shrink = genericShrink
