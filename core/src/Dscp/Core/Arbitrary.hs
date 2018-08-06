-- | Arbitrary and other test-related instances.

module Dscp.Core.Arbitrary
    ( -- * Generators
      genPleasantGrade
    , genStudentSignedSubmissions
    , genCommonAssignmentType
    , genCommonDocumentType

      -- * Examples
    , studentEx
    , studentSKEx
    , courseEx
    , assignmentEx
    , signedSubmissionEx
    , submissionEx
    , gradeEx
    , privateTxEx
    , submissionWitnessEx
    , utcTimeEx
    ) where

import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.IO.Unsafe (unsafePerformIO)

import Dscp.Core.Foundation
import Dscp.Crypto
import Dscp.Util
import Dscp.Util.Test

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
    , (5, arbitrary `suchThat` (> threshold))
    ]
  where
    threshold =
        let UnsafeGrade maxG = maxBound
        in fromMaybe (error "genPleasantGrade: bad grade") $
           mkGrade (maxG `div` 2)

instance Arbitrary Assignment where
    arbitrary =
        Assignment
        <$> arbitrary
        <*> arbitrary
        <*> genCommonAssignmentType
        <*> arbitrary

instance Arbitrary Submission where
    arbitrary = Submission <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SubmissionWitness where
    arbitrary = do
        sk <- arbitrary
        sig <- unsafeSign sk <$> arbitrary @ByteString
        pure $ SubmissionWitness (toPublic sk) sig

-- | Generate several submissions of same student.
-- 'sStudentId' field of generated submission will be replaced.
genStudentSignedSubmissions
    :: Gen SecretKey
    -> Gen Submission
    -> Gen (Student, Assignment, NonEmpty SignedSubmission)
genStudentSignedSubmissions genSK genSubmission = do
    sk <- genSK
    subs <- listOf1 genSubmission `suchThatMap` nonEmpty
    assignment <- arbitrary
    let pk = toPublic sk
        studentId = mkAddr pk
    ss <- forM subs $ \sub -> do
        let sub' = sub & sStudentId .~ studentId
                       & sAssignmentId .~ hash assignment
            sig = sign sk $ hash sub'
        pure $ SignedSubmission sub' $ SubmissionWitness pk sig
    return (studentId, assignment, ss)

instance Arbitrary SignedSubmission where
    arbitrary = do
        (_, _, ss :| _) <- genStudentSignedSubmissions arbitrary arbitrary
        return ss

instance Arbitrary AssignmentType where
    arbitrary = elements [Regular, CourseFinal]

genCommonAssignmentType :: Gen AssignmentType
genCommonAssignmentType = frequency [(5, pure Regular), (1, pure CourseFinal)]

instance Arbitrary DocumentType where
    arbitrary = elements [Offline, Online]

genCommonDocumentType :: Gen DocumentType
genCommonDocumentType = frequency [(5, pure Offline), (1, pure Online)]

instance Arbitrary PrivateTx where
    arbitrary = PrivateTx <$> arbitrary <*> arbitrary <*> arbitrary

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
assignmentEx =
    Assignment
    { _aCourseId = courseEx
    , _aType = Regular
    , _aContentsHash = offlineHash
    , _aDesc = "Find mathematical model of the world"
    }

signedSubmissionEx :: SignedSubmission
signedSubmissionEx = detGen 123 $ do
    let submission = Submission
            { _sStudentId = studentEx
            , _sContentsHash = offlineHash
            , _sAssignmentId = getId assignmentEx
            }
    (_, _, sigsub :| _) <-
        genStudentSignedSubmissions (pure studentSKEx) (pure submission)
    return sigsub

submissionEx :: Submission
submissionEx = _ssSubmission signedSubmissionEx

gradeEx :: Grade
gradeEx = detGen 123 arbitrary

privateTxEx :: PrivateTx
privateTxEx =
    PrivateTx
    { _ptSignedSubmission = signedSubmissionEx
    , _ptGrade = gradeEx
    , _ptTime = utcTimeEx
    }

submissionWitnessEx :: SubmissionWitness
submissionWitnessEx = _ssWitness signedSubmissionEx

----------------------------------------------------------------------------
-- Orphans
----------------------------------------------------------------------------

utcTimeEx :: UTCTime
utcTimeEx = unsafePerformIO getCurrentTime
{-# NOINLINE utcTimeEx #-}
