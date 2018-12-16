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
    , tiListNE
    , tiOne
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
import qualified Data.Text.Buildable
import Data.Time.Clock (UTCTime, getCurrentTime)
import Fmt ((+||), (||+))
import qualified GHC.Exts as Exts
import GHC.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck (resize)
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)
import qualified Text.Show

import Dscp.Core.FairCV
import Dscp.Core.Foundation
import Dscp.Crypto
import Dscp.Util.Test

instance Arbitrary SecretKeyData where
    arbitrary = mkSecretKeyData <$> arbitrary

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

instance Arbitrary ATGSubjectChange where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance Arbitrary ATGDelta where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance Arbitrary PrivateBlockHeader where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance Arbitrary Coin where
    arbitrary = Coin <$> choose (0, 10000)

deriving instance Arbitrary Nonce
deriving instance Arbitrary Difficulty
deriving instance Arbitrary SlotId
deriving instance Arbitrary BlockMetaTx

instance Arbitrary TxInAcc where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance Arbitrary TxOut where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance Arbitrary Tx where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance Arbitrary TxWitness where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance Arbitrary TxWitnessed where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance Arbitrary PublicationTx where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance Arbitrary GTxWitnessed where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance Arbitrary PublicationTxWitness where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance Arbitrary PublicationTxWitnessed where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance Arbitrary GTx where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary GTxId where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Header where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance Arbitrary BlockBody where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance Arbitrary Block where
    arbitrary = genericArbitrary
    shrink    = genericShrink

-- | TODO: resolve the weird problem with _very_ long generation
-- and produce larger FairCVs.
instance Arbitrary FairCV where
    arbitrary = resize 5 $
        FairCV <$> arbitrary <*> arbitrary <*> arbitrary

instance ArbitraryMixture (AbstractSK ss) where
    arbitraryMixture = primitiveArbitraryMixture
instance ArbitraryMixture (AbstractPK ss) where
    arbitraryMixture = primitiveArbitraryMixture
instance ArbitraryMixture (AbstractSig ss a) where
    arbitraryMixture = primitiveArbitraryMixture
instance ArbitraryMixture (AbstractHash ss a) where
    arbitraryMixture = primitiveArbitraryMixture
instance ArbitraryMixture Address where
    arbitraryMixture = primitiveArbitraryMixture
instance ArbitraryMixture Nonce where
    arbitraryMixture = primitiveArbitraryMixture
instance ArbitraryMixture Difficulty where
    arbitraryMixture = primitiveArbitraryMixture
instance ArbitraryMixture SlotId where
    arbitraryMixture = primitiveArbitraryMixture
instance ArbitraryMixture GTxWitnessed where
    arbitraryMixture = primitiveArbitraryMixture
instance ArbitraryMixture Coin where
    arbitraryMixture = primitiveArbitraryMixture
instance ArbitraryMixture ATGDelta where
    arbitraryMixture = primitiveArbitraryMixture
instance ArbitraryMixture (MerkleSignature a) where
    arbitraryMixture = primitiveArbitraryMixture

instance ArbitraryMixture TxInAcc
instance ArbitraryMixture TxOut
instance ArbitraryMixture Tx
instance ArbitraryMixture TxWitness
instance ArbitraryMixture TxWitnessed
instance ArbitraryMixture PrivateBlockHeader
instance ArbitraryMixture PublicationTx
instance ArbitraryMixture PublicationTxWitness
instance ArbitraryMixture PublicationTxWitnessed

instance ArbitraryMixture Header
instance ArbitraryMixture BlockBody
instance ArbitraryMixture Block

instance Arbitrary PgText where
    arbitrary = arbitrary `suchThatMap` (rightToMaybe . mkPgText)

---------------------------------------------------------------------
-- Test case input
---------------------------------------------------------------------

-- | Used to specify how items of given type should be generated.
data TestItemParam a
    = FixedItemSet (Gen (NonEmpty a))
    -- ^ Generate set of items of use them to construct more complex ones,
    -- maybe with repetitions.

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

-- | Simpliest setting: one student, one assignment and so on.
simpleCoreTestParams :: CoreTestParams
simpleCoreTestParams =
    CoreTestParams
    { ctpSecretKey = oneTestItem arbitrary
    , ctpAssignment = oneTestItem arbitrary
    , ctpSubmissionContentsHash = FixedItemSet arbitrary
    }

-- | Everything varies.
wildCoreTestParams :: CoreTestParams
wildCoreTestParams =
    CoreTestParams
    { ctpSecretKey = FixedItemSet arbitrary
    , ctpAssignment = FixedItemSet arbitrary
    , ctpSubmissionContentsHash = FixedItemSet arbitrary
    }

-- | Contains various incarnations of an entity.
-- It is supposed to provide various amount of data depending on used needs:
-- one element, list of elements or even infinite list of elements.
data TestItem a = TestItem
    { tiCycled :: [a]
      -- ^ Cycled list of previously generated items.
    , tiNum    :: Int
      -- ^ Size of the list, if someone asks for a finite list.
    } deriving (Functor, Traversable)

-- | Get a non-empty list of given items. See note to 'tiList'.
tiListNE :: TestItem a -> NonEmpty a
tiListNE TestItem{..} = Exts.fromList $ take tiNum tiCycled

-- | Get a list of given items.
-- List size will be arbitrarly generated. It *may* contain repeated entires.
tiList :: TestItem a -> [a]
tiList = toList . tiListNE

-- | Get a single item.
tiOne :: TestItem a -> a
tiOne = head . tiListNE

instance Foldable TestItem where
    foldr f e ti = foldr f e (tiList ti)

(<+>) :: TestItem (a -> b) -> TestItem a -> TestItem b
t1 <+> t2 =
    TestItem
    { tiCycled = zipWith ($) (tiCycled t1) (tiCycled t2)
    , tiNum = max (tiNum t1) (tiNum t2)
    }
infixl 4 <+>

mkTestItem :: (Arbitrary a) => TestItemParam a -> Gen (TestItem a)
mkTestItem (FixedItemSet genList) = do
    l <- genList
    return TestItem
        { tiCycled = Exts.fromList $ cycleLimited $ toList l
        , tiNum = length l
        }
  where
    -- this helps to prevent hanging when one asks for a list of unique
    -- items, but not enough different elements were generated
    cycleLimited l = fold (replicate 1000 l)
                  ++ error "Generated not enough test items"

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
-- You can make some assumptions about connections between generated data.
-- For instance, @zip (cycle cteCourses) cteAssignments@ will produce pairs
-- of assignments and corresponding courses.
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
