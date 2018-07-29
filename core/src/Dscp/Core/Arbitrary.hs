-- | Arbitrary and other test-related instances.

module Dscp.Core.Arbitrary
    ( -- * Generators
      genPleasantGrade
    , genStudentSignedSubmissions

      -- * Examples
    , studentEx
    , studentSKEx
    , courseEx
    , assignmentEx
    , signedSubmissionEx
    ) where

import Dscp.Util.Test

import Dscp.Core.Serialise ()
import Dscp.Core.Types
import Dscp.Crypto
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
-- 'sStudentId' field of generated submission will be replaced.
genStudentSignedSubmissions
    :: Gen SecretKey
    -> Gen Submission
    -> Gen (Student, NonEmpty SignedSubmission)
genStudentSignedSubmissions genSK genSubmission = do
    sk <- genSK
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

---------------------------------------------------------------------
-- Examples
---------------------------------------------------------------------

-- Usefull to generate swagger examples, for instance.

studentSKEx :: SecretKey
studentSKEx = withIntSeed 123 genSecretKey

studentEx :: Student
studentEx = mkAddr $ toPublic studentSKEx

courseEx :: Course
courseEx = Course 7

assignmentEx :: Assignment
assignmentEx = detGen 123 $ do
    _aContentsHash <- arbitrary
    return Assignment
        { _aCourseId = courseEx
        , _aType = Regular
        , _aDesc = "Find mathematical model of the world"
        , ..
        }

signedSubmissionEx :: SignedSubmission
signedSubmissionEx = detGen 123 $ do
    _sContentsHash <- arbitrary
    let submission = Submission
            { _sStudentId = studentEx
            , _sAssignment = assignmentEx
            , ..
            }
    (_, sigsub :| _) <-
        genStudentSignedSubmissions (pure studentSKEx) (pure submission)
    return sigsub
