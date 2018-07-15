module Test.Dscp.Core.Instances
    ( genStudentSignedSubmissions
    ) where

import Dscp.Core.Serialise ()
import Dscp.Core.Types (Address (..), Assignment (..), AssignmentType (..), Course (..),
                        DocumentType (..), Grade, SignedSubmission (..), Student, Subject (..),
                        Submission (..), SubmissionWitness (..), mkAddr, mkGrade, sStudentId)
import Dscp.Crypto (hash, sign, toPublic, unsafeSign)

import Test.Common
import Test.Dscp.Crypto.Instances ()

instance Arbitrary Address where
    arbitrary = Address <$> arbitrary

instance Arbitrary Course where
    arbitrary = Course <$> arbitrary

instance Arbitrary Subject where
    arbitrary = Subject <$> arbitrary

instance Arbitrary Grade where
    arbitrary = arbitrary `suchThatMap` mkGrade

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
    arbitrary = elements [Regular, CourseFinal]

instance Arbitrary DocumentType where
    arbitrary = elements [Offline, Online]
