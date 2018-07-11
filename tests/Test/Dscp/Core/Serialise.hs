module Test.Dscp.Core.Serialise where

import Dscp.Core.Aeson ()
import Dscp.Core.Serialise ()
import Dscp.Core.Types (Address, Assignment, AssignmentType, Course, DocumentType, Grade,
                        SignedSubmission, Subject, Submission, SubmissionWitness)

import Test.Common
import Test.Dscp.Core.Instances ()
import Test.Dscp.Serialise (aesonRoundtripProp, serialiseRoundtripProp)

spec_coreSerialise :: Spec
spec_coreSerialise = describe "Core datatypes binary serialisation" $ do
    serialiseRoundtripProp @Course
    serialiseRoundtripProp @Subject
    serialiseRoundtripProp @Address
    serialiseRoundtripProp @AssignmentType
    serialiseRoundtripProp @Assignment
    serialiseRoundtripProp @DocumentType
    serialiseRoundtripProp @Grade
    serialiseRoundtripProp @Submission
    serialiseRoundtripProp @SubmissionWitness
    serialiseRoundtripProp @SignedSubmission

spec_coreAeson :: Spec
spec_coreAeson = describe "Core datatypes JSON serialisation" $ do
    aesonRoundtripProp @Course
    aesonRoundtripProp @Subject
    aesonRoundtripProp @Address
    aesonRoundtripProp @AssignmentType
    aesonRoundtripProp @Assignment
    aesonRoundtripProp @DocumentType
    aesonRoundtripProp @Grade
    aesonRoundtripProp @Submission
    aesonRoundtripProp @SubmissionWitness
    aesonRoundtripProp @SignedSubmission
