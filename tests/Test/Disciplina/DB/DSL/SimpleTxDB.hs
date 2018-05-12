module Test.Disciplina.DB.DSL.SimpleTxDB where

import Test.Common

import Crypto.Error (CryptoFailable(..))
import Disciplina.DB (QueryTx(..), QueryTxs(..), WHERE(..)
                     ,TxIdEq(..), TxGrade(..)
                     ,TxsFilterExpr(..), runSimpleTxDBQuery)
import Disciplina.Educator (PrivateTx(..), EducatorTxMsg(..)
                           ,StudentTxMsg(..), PrivateTxPayload(..))
import Disciplina.Crypto (hash, AbstractPK(..), AbstractSK(..), PublicKey, SecretKey)
import Disciplina.Core (CourseId(..), SubjectId, Grade(..), mkAddr)

import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteString.Char8 as C

type StudentKey = Char
type EducatorKey = Char

-- | SubjectIds are taken from Disciplina.Core.ATG
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
  PrivateTx {
       _ptxStudentId = mkAddr . fst $ mkKeyPair studentKey
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

-- | Enroll student in course id sIdMathematics
enrollPrivateTx :: StudentKey -> EducatorKey -> PrivateTx
enrollPrivateTx = mkPrivateTx courseId payload
 where
   courseId = CourseId { ciSubject = sIdMathematics
                       , ciId = 2
                       }
   payload = StudentTx { _ptxStudentMsg = Enroll }

-- | Enroll student 'a' in course id sIdComputerScience at educator 'k'
tx1 :: PrivateTx
tx1 = mkPrivateTx (CourseId sIdComputerScience 3) (StudentTx Enroll) 'a' 'k'

-- | Student 'a' gets graded B by educator 'k' in course id sIdComputerScience
tx2 :: PrivateTx
tx2 = mkPrivateTx (CourseId sIdComputerScience 3) (EducatorTx (GradeCourse B)) 'a' 'k'

-- | Enroll student 'b' in course id sIdElementary at educator 'k'
tx3 :: PrivateTx
tx3 = mkPrivateTx (CourseId sIdElementary 4) (StudentTx Enroll) 'a' 'k'

-- | Student 'b' gets graded C by educator 'k' in course id sIdElementary
tx4 :: PrivateTx
tx4 = mkPrivateTx (CourseId sIdElementary 4) (EducatorTx (GradeCourse C)) 'a' 'k'

simpleTxDB :: [PrivateTx]
simpleTxDB = fmap (uncurry enrollPrivateTx) (zip ['a'..'j'] ['k'..'t'])
             <> [tx1,tx2,tx3,tx4]

spec_Transactions :: Spec
spec_Transactions = describe "SimpleTxDB Query" $ do
    it "Find tx with TxIdEq" $ do
        testQuery10 `shouldBe` Just (enrollPrivateTx 'a' 'k')
        testQuery11 `shouldBe` Nothing
    it "Find txs with TxSubjectIdEq" $ do
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
    it "Find txs with TxSubjectIsDescendantOf" $ do
        testQuery70 `shouldBe` []
        testQuery71 `shouldBe` [tx3,tx4]

-- | Should return Just (enrollPrivateTx 'a' 'k')
testQuery10 :: Maybe PrivateTx
testQuery10 = runSimpleTxDBQuery simpleTxDB query
  where query = SELECTTx WHERE (TxIdEq (hash tx))
        tx = enrollPrivateTx 'a' 'k'

-- | Should return Nothing since enrollPrivateTx 'a' 'l'
-- | does not exist in simpleTxDB
testQuery11 :: Maybe PrivateTx
testQuery11 = runSimpleTxDBQuery simpleTxDB query
  where query = SELECTTx WHERE (TxIdEq (hash tx))
        tx = enrollPrivateTx 'a' 'l'

testQuery20 :: [PrivateTx]
testQuery20 = runSimpleTxDBQuery simpleTxDB query
  where query = SELECTTxs WHERE (TxSubjectIdEq sIdComputerScience)

testQuery21 :: [PrivateTx]
testQuery21 = runSimpleTxDBQuery simpleTxDB query
  where query = SELECTTxs WHERE (TxSubjectIdEq sIdCalculi)

testQuery22 :: [PrivateTx]
testQuery22 = runSimpleTxDBQuery simpleTxDB query
  where query = SELECTTxs WHERE (TxSubjectIdEq sIdComputerScience
                                :|| TxSubjectIdEq sIdElementary)

testQuery30 :: [PrivateTx]
testQuery30 = runSimpleTxDBQuery simpleTxDB query
  where query= SELECTTxs WHERE (TxGrade :>= B)

testQuery31 :: [PrivateTx]
testQuery31 = runSimpleTxDBQuery simpleTxDB query
  where query= SELECTTxs WHERE (TxGrade :>= A)

testQuery40 :: [PrivateTx]
testQuery40 = runSimpleTxDBQuery simpleTxDB query
  where query= SELECTTxs WHERE (TxSubjectIdEq sIdComputerScience :& TxGrade :>= B)

testQuery41 :: [PrivateTx]
testQuery41 = runSimpleTxDBQuery simpleTxDB query
  where query= SELECTTxs WHERE (TxSubjectIdEq sIdElementary :& TxGrade :>= B)

testQuery42 :: [PrivateTx]
testQuery42 = runSimpleTxDBQuery simpleTxDB query
  where query= SELECTTxs WHERE (TxSubjectIdEq sIdElementary :& TxGrade :>= D)

testQuery43 :: [PrivateTx]
testQuery43 = runSimpleTxDBQuery simpleTxDB query
  where query = SELECTTxs WHERE (TxSubjectIdEq sIdComputerScience :& TxGrade :>= A)

testQuery50 :: [PrivateTx]
testQuery50 = runSimpleTxDBQuery simpleTxDB query
  where query = SELECTTxs WHERE (TxSubjectIdEq sIdComputerScience
                                 :& ((TxGrade :>= B) :|| TxSubjectIdEq sIdCalculi))

testQuery60 :: [PrivateTx]
testQuery60 = runSimpleTxDBQuery simpleTxDB query
  where query = SELECTTxs WHERE ((TxSubjectIdEq sIdComputerScience :& TxGrade :>= A)
                                 :|| TxSubjectIdEq sIdComputerScience)

testQuery70 :: [PrivateTx]
testQuery70 = runSimpleTxDBQuery simpleTxDB query
  where query = SELECTTxs WHERE (TxSubjectIsDescendantOf sIdEngineering)

testQuery71 :: [PrivateTx]
testQuery71 = runSimpleTxDBQuery simpleTxDB query
  where query = SELECTTxs WHERE (TxSubjectIsDescendantOf sIdElementary)
