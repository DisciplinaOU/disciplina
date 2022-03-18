module Test.Dscp.Educator.Serialise
    ( spec_Serialisation
    ) where

import Universum
import Dscp.Util.Test

import Dscp.Educator.Resource (KeyJson (..), KeyfileContent)
import Dscp.Educator.Web.Types
import Dscp.Educator.Web.Educator.Types
import Dscp.Educator.Web.Student.Types

import Test.Dscp.Educator.Web.Instances ()

spec_Serialisation :: Spec
spec_Serialisation = describe "Serialisation" $ do
    describe "Aeson" $ do
        describe "roundtrip" $ do
            aesonRoundtripProp @KeyJson
            aesonRoundtripProp @KeyfileContent
            aesonRoundtripProp @IsEnrolled
            aesonRoundtripProp @IsFinal
            aesonRoundtripProp @IsGraded
            aesonRoundtripProp @HasProof
            aesonRoundtripProp @GradeInfo
            aesonRoundtripProp @BlkProofInfo
            aesonRoundtripProp @NewStudent
            aesonRoundtripProp @NewCourse
            aesonRoundtripProp @NewGrade
            aesonRoundtripProp @NewAssignment
            aesonRoundtripProp @NewStudentCourse
            aesonRoundtripProp @NewStudentAssignment
            aesonRoundtripProp @EducatorInfo
            aesonRoundtripProp @CourseEducatorInfo
            aesonRoundtripProp @AssignmentEducatorInfo
            aesonRoundtripProp @SubmissionEducatorInfo
            aesonRoundtripProp @Certificate
            aesonRoundtripProp @CertificateGrade
            aesonRoundtripProp @CertificateFullInfo
            aesonRoundtripProp @(Counted ())
            aesonRoundtripProp @NewSubmission
            aesonRoundtripProp @CourseStudentInfo
            aesonRoundtripProp @AssignmentStudentInfo
            aesonRoundtripProp @SubmissionStudentInfo
