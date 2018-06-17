module Test.Dscp.DB.DSL.Interpret.SimpleTxDB where

import Test.Common

import Crypto.Error (CryptoFailable (..))
import Dscp.Core (CourseId (..), Grade (..), SubjectId, mkAddr)
import Dscp.Crypto (AbstractPK (..), AbstractSK (..), PublicKey, SecretKey, hash)
import Dscp.DB (Obj, ObjHashEq (..), QueryObj (..), QueryTx (..), QueryTxs (..), TxGrade (..),
                      TxIdEq (..), TxsFilterExpr (..), WHERE (..), runSimpleTxDBQuery)
import Dscp.Educator (EducatorTxMsg (..), PrivateTx (..), PrivateTxPayload (..),
                            StudentTxMsg (..))

import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteString.Char8 as C

type StudentKey = Char
type EducatorKey = Char

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
mkPrivateTx :: CourseId -> PrivateTxPayload -> StudentKey -> EducatorKey -> PrivateTx
mkPrivateTx courseId payload studentKey educatorKey =
    PrivateTx { _ptxStudentId = mkAddr . fst $ mkKeyPair studentKey
              , _ptxCourseId = courseId
              , _ptxEducatorId = mkAddr . fst $ mkKeyPair educatorKey
              , _ptxPayload = payload
              }
  where
     -- Create key pair from seed
     mkKeyPair :: Char -> (PublicKey, SecretKey)
     mkKeyPair seed =
       let (CryptoPassed x) = Ed25519.secretKey (C.replicate 32 seed)
       in (AbstractPK (Ed25519.toPublic x), AbstractSK x)

-- | Enroll student in course Mathematics
enrollMathPrivateTx :: StudentKey -> EducatorKey -> PrivateTx
enrollMathPrivateTx = mkPrivateTx courseId payload
  where
    courseId = CourseId 2
    payload = StudentTx { _ptxStudentMsg = Enroll }

-- | Enroll student 'a' in course id 3 at educator 'k'
tx1 :: PrivateTx
tx1 = mkPrivateTx (CourseId 3) (StudentTx Enroll) 'a' 'k'

-- | Student 'a' gets graded B by educator 'k' in course id 3
tx2 :: PrivateTx
tx2 = mkPrivateTx (CourseId 3) (EducatorTx (GradeCourse B)) 'a' 'k'

-- | Enroll student 'b' in course id 4 at educator 'k'
tx3 :: PrivateTx
tx3 = mkPrivateTx (CourseId 4) (StudentTx Enroll) 'a' 'k'

-- | Student 'b' gets graded C by educator 'k' in course id 4
tx4 :: PrivateTx
tx4 = mkPrivateTx (CourseId 4) (EducatorTx (GradeCourse C)) 'a' 'k'

-- | Enroll student 'a' in course id 5 at educator 'k'
tx5 :: PrivateTx
tx5 = mkPrivateTx (CourseId 5) (StudentTx Enroll) 'a' 'k'

simpleTxDB :: [PrivateTx]
simpleTxDB =
    fmap (uncurry enrollMathPrivateTx) (zip ['a'..'j'] ['k'..'t']) <> [tx1, tx2, tx3, tx4, tx5]

obj1 :: Obj
obj1 = "obj1"

simpleObjDB :: [Obj]
simpleObjDB = [obj1]

spec_Transactions :: Spec
spec_Transactions = describe "SimpleTxDB Query" $ do
    it "Find tx with TxIdEq" $ do
        testQuery10 `shouldBe` Just (enrollMathPrivateTx 'a' 'k')
        testQuery11 `shouldBe` Nothing
    it "Find txs with TxHasSubjectId" $ do
        testQuery20 `shouldBe` [tx1,tx2]
        testQuery21 `shouldBe` []
        testQuery22 `shouldBe` [tx1,tx2,tx3,tx4]
    it "Find txs with TxGrade >= " $ do
        testQuery30 `shouldBe` [tx2]
        testQuery31 `shouldBe` []
    it "Find txs with AND combinator " $ do
        testQuery40 `shouldBe` [tx2]
        testQuery41 `shouldBe` []
        testQuery42 `shouldBe` [tx4]
        testQuery43 `shouldBe` []
    it "Find txs with AND and OR combinator " $ do
        testQuery50 `shouldBe` [tx2]
        testQuery60 `shouldBe` [tx1,tx2]
    it "Find txs with TxHasDescendantOfSubjectId" $ do
        testQuery70 `shouldBe` [tx5]
        testQuery71 `shouldBe` [tx1,tx2,tx5]
    it "Find object with ObjHashEq" $ do
        testQuery80 `shouldBe` Nothing
        testQuery81 `shouldBe` (Just obj1)


-- | Should return Just (enrollMathPrivateTx 'a' 'k')
testQuery10 :: Maybe PrivateTx
testQuery10 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTTx WHERE (TxIdEq (hash tx))
        tx = enrollMathPrivateTx 'a' 'k'

-- | Should return Nothing since enrollMathPrivateTx 'a' 'l'
-- | does not exist in simpleTxDB
testQuery11 :: Maybe PrivateTx
testQuery11 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTTx WHERE (TxIdEq (hash tx))
        tx = enrollMathPrivateTx 'a' 'l'

testQuery20 :: [PrivateTx]
testQuery20 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTTxs WHERE (TxHasSubjectId sIdComputerScience)

testQuery21 :: [PrivateTx]
testQuery21 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTTxs WHERE (TxHasSubjectId sIdCalculi)

testQuery22 :: [PrivateTx]
testQuery22 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTTxs WHERE (TxHasSubjectId sIdComputerScience
                                :|| TxHasSubjectId sIdElementary)

testQuery30 :: [PrivateTx]
testQuery30 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query= SELECTTxs WHERE (TxGrade :>= B)

testQuery31 :: [PrivateTx]
testQuery31 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query= SELECTTxs WHERE (TxGrade :>= A)

testQuery40 :: [PrivateTx]
testQuery40 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query= SELECTTxs WHERE (TxHasSubjectId sIdComputerScience :& TxGrade :>= B)

testQuery41 :: [PrivateTx]
testQuery41 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query= SELECTTxs WHERE (TxHasSubjectId sIdElementary :& TxGrade :>= B)

testQuery42 :: [PrivateTx]
testQuery42 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query= SELECTTxs WHERE (TxHasSubjectId sIdElementary :& TxGrade :>= D)

testQuery43 :: [PrivateTx]
testQuery43 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTTxs WHERE (TxHasSubjectId sIdComputerScience :& TxGrade :>= A)

testQuery50 :: [PrivateTx]
testQuery50 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTTxs WHERE (TxHasSubjectId sIdComputerScience
                                 :& ((TxGrade :>= B) :|| TxHasSubjectId sIdCalculi))

testQuery60 :: [PrivateTx]
testQuery60 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTTxs WHERE ((TxHasSubjectId sIdComputerScience :& TxGrade :>= A)
                                 :|| TxHasSubjectId sIdComputerScience)

testQuery70 :: [PrivateTx]
testQuery70 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTTxs WHERE (TxHasDescendantOfSubjectId sIdEngineering)

testQuery71 :: [PrivateTx]
testQuery71 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTTxs WHERE (TxHasDescendantOfSubjectId sIdComputerScience)

testQuery80 :: Maybe Obj
testQuery80 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTObj WHERE (ObjHashEq (hash "does not exists"))

testQuery81 :: Maybe Obj
testQuery81 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTObj WHERE (ObjHashEq (hash obj1))
