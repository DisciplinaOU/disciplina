module Test.Dscp.Core.Instances () where

import Dscp.Core.Serialise ()
import Dscp.Core.Types (Address (..), Assignment (..), AssignmentType (..), Course (..),
                        DocumentType (..), Grade, SignedSubmission (..), Subject (..),
                        Submission (..), SubmissionWitness (..), mkGrade)
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

instance Arbitrary AssignmentType where
    arbitrary = elements [Regular, CourseFinal]

instance Arbitrary DocumentType where
    arbitrary = elements [Offline, Online]
