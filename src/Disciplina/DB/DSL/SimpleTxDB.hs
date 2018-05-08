module Disciplina.DB.DSL.SimpleTxDB () where

import Universum

import Codec.Serialise (Serialise(..))
import Crypto.Error (CryptoFailable(..))

import Disciplina.Crypto.Signing.Class (AbstractPK(..), AbstractSK(..))
import Disciplina.DB.DSL.Types (QueryTx(..), QueryTxs(..), WHERE(..)
                               ,TxIdEq(..), TxGrade(..), QueryObj(..)
                               ,TxsFilterExpr(..), ObjHashEq(..))
import Disciplina.Educator.Txs (PrivateTxId, PrivateTx(..), EducatorTxMsg(..)
                               ,StudentTxMsg(..), PrivateTxPayload(..))
import Disciplina.Crypto (hash, PublicKey, SecretKey)
import Disciplina.Core (Address(..), CourseId(..), SubjectId, Grade(..)
                       ,mkAddr, hasPathFromTo
                       ,activityTypeGraphIndexed)
import Disciplina.DB.DSL.Interpret (MonadSearchTxObj(..), RunQuery(..))
import Data.List (union, intersect)

import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteString.Char8 as C


-- | Simple transaction database
newtype SimpleTxDB a = SimpleTxDB
   { getSimpleTxDB :: Reader [PrivateTx] a
   } deriving (Functor, Applicative, Monad, MonadReader [PrivateTx])

instance MonadSearchTxObj SimpleTxDB where
  runTxQuery = runSimpleTxQuery
  runTxsQuery = runSimpleTxsQuery
  runObjQuery = undefined -- TODO, implement this one, but how are Object defined?

instance Serialise Address where
  encode (Address h) = encode h
  decode = Address <$> decode

-- | TODO, where should we put these?
instance Serialise Grade
instance Serialise EducatorTxMsg
instance Serialise StudentTxMsg
instance Serialise PrivateTxPayload
instance Serialise CourseId
instance Serialise PrivateTx


-- | Find Tx in db with hash == h
runSimpleTxQuery :: QueryTx -> SimpleTxDB (Maybe PrivateTx)
runSimpleTxQuery (SELECTTx _ (TxIdEq (h :: PrivateTxId))) =
  safeHead.filter ((==h).hash) <$> ask

-- | Find Txs in db with SubjectId == a
runSimpleTxsQuery :: QueryTxs -> SimpleTxDB [PrivateTx]
runSimpleTxsQuery (SELECTTxs _ (TxSubjectIdEq sId)) =
  filter ((==sId).ciSubject._ptxCourseId) <$> ask

-- | Find Txs in db with grade == g
runSimpleTxsQuery (SELECTTxs _ (TxGradeEq grade)) =
  filter ((== Just grade).getGrade._ptxPayload) <$> ask
  where getGrade (EducatorTx (GradeCourse g)) = Just g
        getGrade _ = Nothing

-- | Find Txs in db with grade >= g
runSimpleTxsQuery (SELECTTxs _ (_ :>= grade)) = do
  filter ((>= Just grade).getGrade._ptxPayload) <$> ask
  where getGrade (EducatorTx (GradeCourse g)) = Just g
        getGrade _ = Nothing

runSimpleTxsQuery (SELECTTxs _ (a :& b)) =
   intersect <$> runQuery (SELECTTxs WHERE a) <*> runQuery (SELECTTxs WHERE b)

runSimpleTxsQuery (SELECTTxs _ (a :|| b)) =
   union <$> runQuery (SELECTTxs WHERE a) <*> runQuery (SELECTTxs WHERE b)

-- | Find all txs with subjectId which is descendant of sId
runSimpleTxsQuery (SELECTTxs _ (TxSubjectIsDescendantOf sId)) =
  filter (isDescendantOf sId . ciSubject ._ptxCourseId) <$> ask
  where isDescendantOf x y = hasPathFromTo activityTypeGraphIndexed x y


-- | run query
-- given <tx_hash>, I want to be able to get { <tx> | hash(<tx>) = <tx_hash> }
runFindTx :: PrivateTx -> Maybe PrivateTx
runFindTx tx = runReader (getSimpleTxDB . runQuery $ q tx) simpleTxDB
  where q tx = SELECTTx WHERE (TxIdEq (hash tx))

-- | Should return Nothing since mkPrivateTx 'a' 'l'
-- | does not exist in privateTx db
testFindTx1 :: Maybe PrivateTx
testFindTx1 = runFindTx $ mkPrivateTx 'a' 'l'

-- | Should return Just (mkPrivate 'a' 'k')
testFindTx2 :: Maybe PrivateTx
testFindTx2 = runFindTx $ mkPrivateTx 'a' 'k'


-- | run query
-- | I want to be able to get { [<tx>] | P(<ordered_tx_param>) }, where P is a system of (in)equalities
runFindTxs :: QueryTxs -> [PrivateTx]
runFindTxs q = runReader (getSimpleTxDB . runQuery $ q) simpleTxDB

-- | TODO, move tests to test module
txsQuery1 :: QueryTxs
txsQuery1 = SELECTTxs WHERE (TxSubjectIdEq 1)

txsQuery2 :: QueryTxs
txsQuery2 = SELECTTxs WHERE (TxSubjectIdEq 2)

txsQuery3 :: QueryTxs
txsQuery3 = SELECTTxs WHERE (TxGrade :>= B)

txsQuery4 :: QueryTxs
txsQuery4 = SELECTTxs WHERE (TxSubjectIdEq 2 :& TxGrade :>= B)

txsQuery5 :: QueryTxs
txsQuery5 = SELECTTxs WHERE (TxSubjectIdEq 2 :& TxGrade :>= A)

txsQuery6 :: QueryTxs
txsQuery6 = SELECTTxs WHERE (TxSubjectIdEq 2 :& ((TxGrade :>= A) :|| TxSubjectIdEq 4))

txsQuery7 :: QueryTxs
txsQuery7 = SELECTTxs WHERE ((TxSubjectIdEq 2 :& TxGrade :>= A) :|| TxSubjectIdEq 2)

txsQuery8 :: QueryTxs
txsQuery8 = SELECTTxs WHERE ((TxSubjectIdEq 2 :& TxGrade :>= A) :& TxSubjectIsDescendantOf 2)


simpleTxDB :: [PrivateTx]
simpleTxDB = fmap (uncurry mkPrivateTx) (zip ['a'..'j'] ['k'..'t'])

type StudentKey = Char
type EducatorKey = Char

-- | Create a simple private transaction
mkPrivateTx :: StudentKey -> EducatorKey -> PrivateTx
mkPrivateTx studentKey educatorKey = PrivateTx {
       _ptxStudentId = mkAddr . fst $ mkKeyPair studentKey
     , _ptxCourseId = courseId
     , _ptxEducatorId = mkAddr . fst $ mkKeyPair educatorKey
     , _ptxPayload = payload2
     }
 where
   courseId = CourseId { ciSubject = 1
                       , ciId = 2
                       }
   payload1 = StudentTx { _ptxStudentMsg  = Enroll }
   payload2 = EducatorTx { _ptxEducatorMsg  = GradeCourse B }

-- | Create key pair from seed
mkKeyPair :: Char -> (PublicKey, SecretKey)
mkKeyPair seed =
  let (CryptoPassed x) = Ed25519.secretKey (C.replicate 32 seed)
  in (AbstractPK (Ed25519.toPublic x), AbstractSK x)

