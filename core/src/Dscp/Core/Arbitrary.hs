{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Arbitrary and other test-related instances.

module Dscp.Core.Arbitrary
    ( -- * Generators
      genPleasantGrade
    , genCommonAssignmentType
    , genCommonDocumentType

      -- * Test inputs
    , CoreTestParams (..)
    , TestItemParam (..)
    , CoreTestEnv (..)
    , TestItem (..)
    , mkTestItem
    , oneTestItem
    , variousItems
    , tiList
    , tiInfUnique
    , genCoreTestEnv
    , cteSubmissions
    , simpleCoreTestParams
    , wildCoreTestParams

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

import qualified Data.Foldable
import Data.List (nub)
import qualified Data.Text.Buildable
import Data.Time.Clock (UTCTime, getCurrentTime)
import Fmt ((+||), (||+))
import qualified GHC.Exts as Exts
import GHC.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck (sized)
import qualified Text.Show

import Dscp.Core.Foundation
import Dscp.Crypto
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

instance Arbitrary SignedSubmission where
    arbitrary = tiOne . cteSignedSubmissions <$> genCoreTestEnv simpleCoreTestParams

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
-- Test case input
---------------------------------------------------------------------

data TestItemParam a
    = FixedItemSet (Gen (NonEmpty a))
    -- ^ Generate set of items of use them to construct more complex ones,
    -- maybe with repetitions.
    | AllRandomItems
    -- ^ Generate each new item from scratch.

-- | Use one item everywhere.
oneTestItem :: Gen a -> TestItemParam a
oneTestItem gen = FixedItemSet (one <$> gen)

-- | Use multiple items, but those may repeat.
variousItems :: Arbitrary a => TestItemParam a
variousItems = FixedItemSet arbitrary

-- | Parameters to generate 'CoreTestEnv'.
data CoreTestParams = CoreTestParams
    { ctpSecretKey              :: TestItemParam SecretKey
      -- ^ Student secret key generation.
    , ctpAssignment             :: TestItemParam Assignment
      -- ^ Assignment generation.
    , ctpSubmissionContentsHash :: TestItemParam (Hash Raw)
      -- ^ Contents hash to use in submission generation.
    }

-- | Simpliest setting: one student, one course and so on.
simpleCoreTestParams :: CoreTestParams
simpleCoreTestParams =
    CoreTestParams
    { ctpSecretKey = oneTestItem arbitrary
    , ctpAssignment = oneTestItem arbitrary
    , ctpSubmissionContentsHash = FixedItemSet arbitrary
    }

-- | Total randomness.
wildCoreTestParams :: CoreTestParams
wildCoreTestParams =
    CoreTestParams
    { ctpSecretKey = AllRandomItems
    , ctpAssignment = AllRandomItems
    , ctpSubmissionContentsHash = AllRandomItems
    }

-- | Contains various incarnations of an entity.
data TestItem a = TestItem
    { tiOne    :: a
      -- ^ Sometime you need only one
    , tiListNE :: NonEmpty a
      -- ^ Or a list of arbitrary length
    , tiInf    :: [a]
      -- ^ Or 3 items
    } deriving (Functor, Traversable)

-- | Usually more convenient.
tiList :: TestItem a -> [a]
tiList = toList . tiListNE

-- | Take several unique items.
-- Make sure 'AllRandomItems' is used as corresponding parameter-generator.
tiInfUnique :: Eq a => TestItem a -> [a]
tiInfUnique = nub . tiInf

instance Foldable TestItem where
    foldr f e ti = foldr f e (tiList ti)

(<+>) :: TestItem (a -> b) -> TestItem a -> TestItem b
t1 <+> t2 =
    TestItem
    { tiOne    = tiOne t1 (tiOne t2)
    , tiListNE = Exts.fromList $
                 smartMerge (toList $ tiListNE t1) (toList $ tiListNE t2)
    , tiInf = zipWith ($) (tiInf t1) (tiInf t2)
    }
  where
    smartMerge l1 l2
        | length l1 <= length l2 = zipWith ($) (cycle l1) l2
        | otherwise = zipWith ($) l1 (cycle l2)
infixl 4 <+>

mkTestItem :: (Arbitrary a) => TestItemParam a -> Gen (TestItem a)
mkTestItem (FixedItemSet genList) = do
    l <- genList
    return TestItem
        { tiOne = head l
        , tiListNE = l
        , tiInf = Exts.fromList $ fold $ repeatLimited $ toList l
        }
  where
    -- to prevent 'tiInfUnique' call from hanging when not enough different
    -- items
    repeatLimited = replicate 100
mkTestItem AllRandomItems = do
    inf <- infiniteList
    n <- sized $ \n -> choose (1, max 1 n)
    return TestItem
        { tiOne = head $ Exts.fromList inf
        , tiListNE = Exts.fromList $ take n inf
        , tiInf = inf
        }

-- | Keeps consistent set of test case data.
-- In wild there is no object containing all the data we may like to have
-- in test case. For instance, 'SignedSubmission' contains no 'Assignment' but
-- a hash of it.
-- Fields are not strict on purpose - due to lazy nature of 'Gen' monad, only
-- needed fields will be generated.
data CoreTestEnv = CoreTestEnv
    { cteStudents          :: TestItem Student
    , cteCourses           :: TestItem Course
    , cteAssignments       :: TestItem Assignment
    , cteSignedSubmissions :: TestItem SignedSubmission
    , ctePrivateTxs        :: TestItem PrivateTx
    }

cteSubmissions :: CoreTestEnv -> TestItem Submission
cteSubmissions = fmap _ssSubmission . cteSignedSubmissions

instance Show CoreTestEnv where
    show = toString . pretty

instance Buildable CoreTestEnv where
    build env@CoreTestEnv{..} =
        "Students: "    +|| tiList cteStudents ||+ "\n\
        \Courses: "     +|| tiList cteCourses ||+ "\n\
        \Assignments: " +|| tiList cteAssignments ||+ "\n\
        \Submissions: " +|| tiList (cteSubmissions env) ||+ "\n\
        \Private txs: " +|| tiList ctePrivateTxs ||+ ""

-- | Generate 'CoreTestEnv'.
--
-- @st@ and @at@ should be either 'NonEmpty' or 'Identity'.
-- In case of 'NonEmpty', different items will be used to constuct each of
-- product items, if enough of them (basic items) there generated.
--
-- You can make some assumptions about connections between generated data.
-- For instance, when 'wildCoreTestParams' is used then i-th student would
-- corresponse to i-th assignment, submission and private transaction.
genCoreTestEnv :: CoreTestParams -> Gen CoreTestEnv
genCoreTestEnv p = do
    sksItem <- mkTestItem $ ctpSecretKey p
    let pksItem = fmap toPublic sksItem
    let studentsItem = fmap mkAddr pksItem
    assignmentsItem <- mkTestItem $ ctpAssignment p
    contentsHashesItem <- mkTestItem $ ctpSubmissionContentsHash p
    let submissionsItem =
          Submission
          <$> studentsItem
          <+> contentsHashesItem
          <+> (hash <$> assignmentsItem)
    let mkSigSub sk pk sub =
          let sig = sign sk $ hash sub
          in SignedSubmission sub $ SubmissionWitness pk sig
    let sigSubsItem =
          mkSigSub
          <$> sksItem
          <+> pksItem
          <+> submissionsItem
    txsItem <- forM sigSubsItem $ \sigSub ->
        PrivateTx <$> pure sigSub <*> arbitrary <*> arbitrary
    return CoreTestEnv
        { cteStudents             = studentsItem
        , cteCourses              = fmap _aCourseId assignmentsItem
        , cteAssignments          = assignmentsItem
        , cteSignedSubmissions    = sigSubsItem
        , ctePrivateTxs           = txsItem
        }

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
signedSubmissionEx = detGen 123 $
    let param = simpleCoreTestParams
                { ctpSubmissionContentsHash = oneTestItem (pure offlineHash) }
    in tiOne . cteSignedSubmissions <$> genCoreTestEnv param

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
