module Test.Dscp.DB.DSL.Interpret.SimpleTxDB where

import Test.Hspec

import Dscp.Core (Course (..), Subject, gA, gB, gC, gD)
import Dscp.Core.Foundation.Educator (PrivateTx (..))
import Dscp.Core.Test
import Dscp.Crypto (PublicKey, SecretKey, hash)
import Dscp.Educator.DB.DSL (Obj, ObjHashEq (..), QueryObj (..), QueryTx (..), QueryTxs (..),
                             TxGrade (..), TxIdEq (..), TxsFilterExpr (..), WHERE (..))
import Dscp.Educator.DB.DSL.Interpret.SimpleTxDB (runSimpleTxDBQuery)
import Dscp.Util (HasId (Id))

-- | Made up courses
courseLinearAlg, courseCompScience1, courseCalculi, courseLogic :: Id Course
courseLinearAlg = Course 2
courseCompScience1 = Course 3
courseCalculi = Course 4
courseLogic = Course 5

-- | SubjectIds are taken from Dscp.Core.ATG
sIdMathematics, sIdComputerScience, sIdElementary
   ,sIdCalculi, sIdLogic, sIdEngineering
   ,sIdTheory, sIdHighSchoolAlgebra, sIdPiCalculus
   ,sIdComputabilityTheory :: Id Subject
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

studentAPubKey, studentBPubKey :: PublicKey
studentAPubKey = mkPubKey 'a'
studentBPubKey = mkPubKey 'b'

educatorKKeyPair :: (PublicKey, SecretKey)
educatorKKeyPair = mkKeyPair 'k'

type StudentKeySeed = Char
type EducatorKeySeed = Char

-- | Grade student an C in course linear alg
mkLinAlgPrivateTx :: StudentKeySeed -> EducatorKeySeed -> PrivateTx
mkLinAlgPrivateTx sKeySeed eKeySeed =
    let studentKey = mkPubKey sKeySeed
        educatorKeyPair = mkKeyPair eKeySeed
    in mkTestPrivateTx courseLinearAlg gC studentKey educatorKeyPair

-- | Educator 'k' grade student 'a' an B in course Computer science
tx1 :: PrivateTx
tx1 = mkTestPrivateTx courseCompScience1 gB studentAPubKey educatorKKeyPair

-- | Educator 'k' grade student 'a' an D by in course Computer science
tx2 :: PrivateTx
tx2 = mkTestPrivateTx courseCompScience1 gD studentAPubKey educatorKKeyPair

-- | Educator 'k' grade student 'b' an D in course Calculi
tx3 :: PrivateTx
tx3 = mkTestPrivateTx courseCalculi gD studentAPubKey educatorKKeyPair

-- | Educator 'k' grade studet 'b' a C in course Calculi
tx4 :: PrivateTx
tx4 = mkTestPrivateTx courseCalculi gC studentAPubKey educatorKKeyPair

-- | Educator 'k' grade student 'a' an D in course logic
tx5 :: PrivateTx
tx5 = mkTestPrivateTx courseLogic gD studentAPubKey educatorKKeyPair

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

disabled_spec_Transactions :: Spec
disabled_spec_Transactions = describe "SimpleTxDB Query" $ do
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
  where query= SELECTTxs WHERE (TxGrade :>= gB)

-- | Query transactions which have grade >= A
testQuery31 :: [PrivateTx]
testQuery31 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query= SELECTTxs WHERE (TxGrade :>= gA)

-- | Query transactions which have grade >= B and have subject id sIdComputerScience
testQuery40 :: [PrivateTx]
testQuery40 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query= SELECTTxs WHERE (TxHasSubjectId sIdComputerScience :& TxGrade :>= gB)

-- | Query transactions which have grade >= B and subject id sIdElementary
testQuery41 :: [PrivateTx]
testQuery41 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query= SELECTTxs WHERE (TxHasSubjectId sIdElementary :& TxGrade :>= gB)

-- | Query transactions which have grade >= D and have subject id sIdElementary
testQuery42 :: [PrivateTx]
testQuery42 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query= SELECTTxs WHERE (TxHasSubjectId sIdElementary :& TxGrade :>= gD)

-- | Query transactions which have grade >= A and subject id sIdComputerScience
testQuery43 :: [PrivateTx]
testQuery43 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTTxs WHERE (TxHasSubjectId sIdComputerScience :& TxGrade :>= gA)

-- | Query transactions which have subject id sIdComputerScience
-- and grade >= B or subject id sIdCalculi
testQuery50 :: [PrivateTx]
testQuery50 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTTxs WHERE (TxHasSubjectId sIdComputerScience
                                 :& ((TxGrade :>= gB) :|| TxHasSubjectId sIdCalculi))

-- | Query transactions which have subject id sIdComputerScience
-- |and grade >= D or subject id sIdEngineering
testQuery60 :: [PrivateTx]
testQuery60 = runSimpleTxDBQuery simpleTxDB simpleObjDB query
  where query = SELECTTxs WHERE ((TxHasSubjectId sIdComputerScience :& TxGrade :>= gD)
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
