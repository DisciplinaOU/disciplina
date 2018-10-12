module Test.Dscp.Core.Serialise where

import Dscp.Core
import Dscp.Util.Test

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
    serialiseRoundtripProp @ATGSubjectChange
    serialiseRoundtripProp @ATGDelta
    serialiseRoundtripProp @PrivateBlockHeader

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
    aesonRoundtripProp @ATGSubjectChange
    aesonRoundtripProp @ATGDelta
    aesonRoundtripProp @Tx
    aesonRoundtripProp @PublicationTx
    aesonRoundtripProp @GTx

spec_coreHttpApi :: Spec
spec_coreHttpApi = describe "Core datatypes HttpApiData serialisation" $ do
    httpApiRoundtripProp @Address
    httpApiRoundtripProp @GTxId
