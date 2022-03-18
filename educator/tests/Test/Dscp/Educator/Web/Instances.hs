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
deriving instance Arbitrary IsGraded
deriving instance Arbitrary IsEnrolled
deriving instance Arbitrary HasProof

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

instance Arbitrary NewStudent where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary NewCourse where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary NewGrade where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary NewAssignment where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary NewStudentCourse where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary NewStudentAssignment where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary EducatorInfo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary CourseEducatorInfo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary AssignmentEducatorInfo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary SubmissionEducatorInfo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Certificate where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary a => Arbitrary (Counted a) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary NewSubmission where
    arbitrary = genericArbitrary
    shrink = genericShrink
