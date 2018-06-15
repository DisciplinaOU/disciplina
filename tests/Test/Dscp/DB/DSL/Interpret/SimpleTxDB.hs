module Test.Dscp.DB.DSL.Interpret.SimpleTxDB where

import Test.Common

import Crypto.Error (CryptoFailable (..))
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import Dscp.Core (Assignment (..), AssignmentType (..), CourseId (..), Grade (..),
                  SignedSubmission (..), SubjectId, Submission (..), SubmissionType (..),
                  SubmissionWitness (..), mkAddr)
import Dscp.Crypto (AbstractPK (..), AbstractSK (..), PublicKey, SecretKey, hash, sign)
import Dscp.DB (Obj, ObjHashEq (..), QueryObj (..), QueryTx (..), QueryTxs (..), TxGrade (..),
                TxIdEq (..), TxsFilterExpr (..), WHERE (..), runSimpleTxDBQuery)
import Dscp.Educator (PrivateTx (..))

import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteString.Char8 as C

type StudentKey = Char
type EducatorKey = Char

-- | Made up courses
courseLinearAlg, courseCompScience1, courseCalculi, courseLogic :: CourseId
courseLinearAlg = CourseId 2
courseCompScience1 = CourseId 3
courseCalculi = CourseId 4
courseLogic = CourseId 5

-- | SubjectIds are taken from Dscp.Core.ATG
sIdMathematics, sIdComputerScience, sIdElementary
   ,sIdCalculi, sIdLogic, sIdEngineering
   ,sIdTheory, sIdHighSchoolAlgebra, sIdPiCalculus
   ,sIdComputabilityTheory :: SubjectId
sIdMathematics = 1
sIdComputerScience = 2
sIdElementary = 3
sIdCalculi = 4
sIdLogic = 5
sIdEngineering = 6
sIdTheory = 7
sIdHighSchoolAlgebra = 8
sIdPiCalculus = 9
sIdComputabilityTheory = 10

-- | Create a private transaction
mkPrivateTx :: CourseId -> Grade -> StudentKey -> EducatorKey -> PrivateTx
mkPrivateTx courseId grade studentKey educatorKey =
    PrivateTx { _ptxSignedSubmission = mkSignedSubmission
              , _ptxGrade = grade
              , _ptxTime = time
              }
  where
     time :: UTCTime
     time = parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" "2018-03-04"

     mkSignedSubmission :: SignedSubmission
     mkSignedSubmission = SignedSubmission
       { ssSubmission = mkSubmission
       , ssWitness = mkSubmissionWitness
       }

     mkSubmission :: Submission
     mkSubmission = Submission
       { sStudentId = mkAddr . fst $ mkKeyPair studentKey
       , sType = Digital
       , sAssignment = mkAssignment
       }

     mkSubmissionWitness :: SubmissionWitness
     mkSubmissionWitness = SubmissionWitness
       { _swKey = fst (mkKeyPair educatorKey)
       , _swSig = sign (snd (mkKeyPair educatorKey)) (hash mkSubmission)
       }

     mkAssignment :: Assignment
     mkAssignment = Assignment
       { aCourseId = courseId
       , aType = Regular
       , aAssignment = ""
       }

     -- Create key pair from seed
     mkKeyPair :: Char -> (PublicKey, SecretKey)
     mkKeyPair seed =
       let (CryptoPassed x) = Ed25519.secretKey (C.replicate 32 seed)
       in (AbstractPK (Ed25519.toPublic x), AbstractSK x)

-- | Grade student a C in course linear alg
mkLinAlgPrivateTx :: StudentKey -> EducatorKey -> PrivateTx
mkLinAlgPrivateTx = mkPrivateTx courseLinearAlg C

-- | Educator 'k' grade student 'a' an A in course Computer science
tx1 :: PrivateTx
tx1 = mkPrivateTx courseCompScience1 B 'a' 'k'

-- | Educator 'k' grade student 'a' an D by in course Computer science
tx2 :: PrivateTx
tx2 = mkPrivateTx courseCompScience1 D 'a' 'k'

-- | Educator 'k' grade student 'b' an D in course Calculi
tx3 :: PrivateTx
tx3 = mkPrivateTx courseCalculi D 'a' 'k'

-- | Educator 'k' grade studet 'b' a C in course Calculi
tx4 :: PrivateTx
tx4 = mkPrivateTx courseCalculi C 'a' 'k'

-- | Educator 'k' grade student 'a' an D in course logic
tx5 :: PrivateTx
tx5 = mkPrivateTx courseLogic D 'a' 'k'

-- | Create a bunch of transactions where student gets graded D in course Linear alg
txs :: [PrivateTx]
txs = fmap (uncurry mkLinAlgPrivateTx) (zip ['a'..'j'] ['k'..'t'])

-- | Transactions used in test database
simpleTxDB :: [PrivateTx]
simpleTxDB = txs <> [tx1, tx2, tx3, tx4, tx5]

-- | A simple object
obj1 :: Obj
obj1 = "obj1"

-- | Objects used in test database
simpleObjDB :: [Obj]
simpleObjDB = [obj1]

spec_Transactions :: Spec
spec_Transactions = describe "SimpleTxDB Query" $ do
    it "Find tx with TxIdEq" $ do
        testQuery10 `shouldBe` Just (mkLinAlgPrivateTx 'a' 'k')
        testQuery11 `shouldBe` Nothing
    it "Find txs with TxHasSubjectId" $ do
        testQuery20 `shouldBe` [tx1, tx2]
        testQuery21 `shouldBe` []
        testQuery22 `shouldBe` [tx1, tx2, tx3, tx4]
    it "Find txs with TxGrade >= " $ do
        testQuery30 `shouldBe` [tx1]
        testQuery31 `shouldBe` []
    it "Find txs with AND combinator " $ do
        testQuery40 `shouldBe` [tx1]
        testQuery41 `shouldBe` []
        testQuery42 `shouldBe` [tx3, tx4]
        testQuery43 `shouldBe` []
    it "Find txs with AND and OR combinator " $ do
        testQuery50 `shouldBe` [tx1]
        testQuery60 `shouldBe` [tx1, tx2, tx5]
    it "Find txs with TxHasDescendantOfSubjectId" $ do
        testQuery70 `shouldBe` [tx5]
        testQuery71 `shouldBe` [tx1, tx2, tx5]
    it "Find object with ObjHashEq" $ do
        testQuery80 `shouldBe` Nothing
        testQuery81 `shouldBe` (Just obj1)


-- | Should return Just (mkLinAlgPrivateTx 'a' 'k')
testQuery10 :: Maybe PrivateTx
testQuery10 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTTx WHERE (TxIdEq (hash tx))
        tx = mkLinAlgPrivateTx 'a' 'k'

-- | Should return Nothing since mkLinAlgPrivateTx 'a' 'l'
-- | does not exist in simpleTxDB
testQuery11 :: Maybe PrivateTx
testQuery11 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTTx WHERE (TxIdEq (hash tx))
        tx = mkLinAlgPrivateTx 'a' 'l'

-- | Query all transactions which have subject id sIdComputerScience
testQuery20 :: [PrivateTx]
testQuery20 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTTxs WHERE (TxHasSubjectId sIdComputerScience)

-- | Query all transactions which have subject id sIdCalculi
testQuery21 :: [PrivateTx]
testQuery21 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTTxs WHERE (TxHasSubjectId sIdCalculi)

testQuery22 :: [PrivateTx]
testQuery22 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTTxs WHERE (TxHasSubjectId sIdComputerScience
                                :|| TxHasSubjectId sIdElementary)

-- | Query transactions which have grade >= B
testQuery30 :: [PrivateTx]
testQuery30 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query= SELECTTxs WHERE (TxGrade :>= B)

-- | Query transactions which have grade >= A
testQuery31 :: [PrivateTx]
testQuery31 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query= SELECTTxs WHERE (TxGrade :>= A)

-- | Query transactions which have grade >= B and have subject id sIdComputerScience
testQuery40 :: [PrivateTx]
testQuery40 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query= SELECTTxs WHERE (TxHasSubjectId sIdComputerScience :& TxGrade :>= B)

-- | Query transactions which have grade >= B and subject id sIdElementary
testQuery41 :: [PrivateTx]
testQuery41 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query= SELECTTxs WHERE (TxHasSubjectId sIdElementary :& TxGrade :>= B)

-- | Query transactions which have grade >= D and have subject id sIdElementary
testQuery42 :: [PrivateTx]
testQuery42 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query= SELECTTxs WHERE (TxHasSubjectId sIdElementary :& TxGrade :>= D)

-- | Query transactions which have grade >= A and subject id sIdComputerScience
testQuery43 :: [PrivateTx]
testQuery43 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTTxs WHERE (TxHasSubjectId sIdComputerScience :& TxGrade :>= A)

-- | Query transactions which have subject id sIdComputerScience
-- and grade >= B or subject id sIdCalculi
testQuery50 :: [PrivateTx]
testQuery50 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTTxs WHERE (TxHasSubjectId sIdComputerScience
                                 :& ((TxGrade :>= B) :|| TxHasSubjectId sIdCalculi))

-- | Query transactions which have subject id sIdComputerScience
-- |and grade >= D or subject id sIdEngineering
testQuery60 :: [PrivateTx]
testQuery60 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTTxs WHERE ((TxHasSubjectId sIdComputerScience :& TxGrade :>= D)
                                 :|| TxHasSubjectId sIdEngineering)

-- | Query transactions which subject id sIdEngineering in its spine
testQuery70 :: [PrivateTx]
testQuery70 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTTxs WHERE (TxHasDescendantOfSubjectId sIdEngineering)

-- | Query transactions which subject id sIdComputerScience in its spine
testQuery71 :: [PrivateTx]
testQuery71 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTTxs WHERE (TxHasDescendantOfSubjectId sIdComputerScience)

-- | Query transactions for object that do not exist
testQuery80 :: Maybe Obj
testQuery80 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTObj WHERE (ObjHashEq (hash "does not exists"))

-- | Query transactions for object with hash of obj1
testQuery81 :: Maybe Obj
testQuery81 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTObj WHERE (ObjHashEq (hash obj1))
