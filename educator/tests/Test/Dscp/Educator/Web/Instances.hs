module Test.Dscp.Educator.Web.Instances
    ( genCourseNoSubjects
    ) where

import Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import Dscp.DB.SQLite
import Dscp.Educator.Web.Educator.Queries
import Dscp.Educator.Web.Student.Queries
import Dscp.Educator.Web.Types (IsFinal (..))
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
