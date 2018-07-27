-- | Arbitrary and other test-related instances.

module Dscp.Core.Arbitrary
    ( genPleasantGrade
    , genStudentSignedSubmissions
    ) where

import Dscp.Util.Test

import Dscp.Core.Serialise ()
import Dscp.Core.Types
import Dscp.Crypto (hash, sign, toPublic, unsafeSign)

instance Arbitrary Address where
    arbitrary = Address <$> arbitrary

instance Arbitrary Course where
    arbitrary = Course <$> arbitrary

instance Arbitrary Subject where
    arbitrary = Subject <$> arbitrary

instance Arbitrary Grade where
    arbitrary = arbitrary `suchThatMap` mkGrade

-- | Let's not make users upset ;)
genPleasantGrade :: Gen Grade
genPleasantGrade =
    frequency
    [ (1, arbitrary)
    , (5, arbitrary `suchThat` (> threashold))
    ]
  where
    threashold =
        let UnsafeGrade maxG = maxBound
        in fromMaybe (error "genPleasantGrade: bad grade") $
           mkGrade (maxG `div` 2)

instance Arbitrary Assignment where
    arbitrary = Assignment <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Submission where
    arbitrary = Submission <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SubmissionWitness where
    arbitrary = do
        sk <- arbitrary
        sig <- unsafeSign sk <$> arbitrary @ByteString
        pure $ SubmissionWitness (toPublic sk) sig

instance Arbitrary SignedSubmission where
    arbitrary = do
        sk <- arbitrary
        sub <- arbitrary
        let pk = toPublic sk
            sig = sign sk $ hash sub
        pure $ SignedSubmission sub $ SubmissionWitness pk sig

-- | Generate several submissions of same student.
genStudentSignedSubmissions
    :: Gen Submission
    -> Gen (Student, NonEmpty SignedSubmission)
genStudentSignedSubmissions genSubmission = do
    sk <- arbitrary
    subs <- listOf1 genSubmission `suchThatMap` nonEmpty
    let pk = toPublic sk
        studentId = mkAddr pk
    ss <- forM subs $ \sub -> do
        let sub' = sub & sStudentId .~ studentId
            sig = sign sk $ hash sub'
        pure $ SignedSubmission sub' $ SubmissionWitness pk sig
    return (studentId, ss)

instance Arbitrary AssignmentType where
    arbitrary = frequency [(5, pure Regular), (1, pure CourseFinal)]

instance Arbitrary DocumentType where
    arbitrary = frequency [(1, pure Offline), (5, pure Online)]
