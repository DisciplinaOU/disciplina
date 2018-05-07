module Disciplina.DB.DSL.SimpleTxDB () where

import Universum

import Codec.Serialise (Serialise(..))
import Control.Lens (filtered, traversed)
import Crypto.Error (CryptoFailable(..))

import Disciplina.Crypto.Signing.Class (AbstractPK(..), AbstractSK(..))
import Disciplina.DB.DSL.Types (QueryTx(..), QueryTxs(..), WHERE(..)
                               ,TxIdEq(..), TxGrade(..), QueryObj(..)
                               ,TxsFilterExpr(..), ObjHashEq(..))
import Disciplina.Educator.Txs (PrivateTxId(..), PrivateTx(..), EducatorTxMsg(..)
                               ,StudentTxMsg(..), PrivateTxPayload(..))
import Disciplina.Crypto (Hash, hash, PublicKey, SecretKey)
import Disciplina.Core (Address(..), CourseId(..), mkAddr, AssignmentId, StudentId)
import Disciplina.DB.DSL.Interpret (MonadSearchTxObj(..), RunQuery(..))
import Data.List (union)

import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Disciplina.Core as Core (Grade(..), SubjectId(..))
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
instance Serialise Core.Grade
instance Serialise EducatorTxMsg
instance Serialise StudentTxMsg
instance Serialise PrivateTxPayload
instance Serialise CourseId
instance Serialise PrivateTx


-- | interpreter for SELECTTx
runSimpleTxQuery :: QueryTx -> SimpleTxDB (Maybe PrivateTx)
runSimpleTxQuery (SELECTTx _ (TxIdEq (h :: PrivateTxId))) = do
  -- find Tx in db with hash == h
  txs <- ask
  return $ txs ^? traversed . filtered ((==h).hash)


-- | interpreter for SELECTTxs
runSimpleTxsQuery :: QueryTxs -> SimpleTxDB [PrivateTx]
runSimpleTxsQuery (SELECTTxs _ (TxSubjectIdEq (sId :: Core.SubjectId))) =
  -- find Txs in db with SubjectId == a
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
   union <$> runQuery (SELECTTxs WHERE a) <*> runQuery (SELECTTxs WHERE b)

runSimpleTxsQuery (SELECTTxs _ (a :|| b)) =
   (<>) <$> runQuery (SELECTTxs WHERE a) <*> runQuery (SELECTTxs WHERE b)

-- TODO, implement this one
runSimpleTxsQuery (SELECTTxs _ (TxSubjectIsDescendantOf a)) = return []


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

txsQuery1 :: QueryTxs
txsQuery1 = SELECTTxs WHERE (TxSubjectIdEq 1)

txsQuery2 :: QueryTxs
txsQuery2 = SELECTTxs WHERE (TxSubjectIdEq 2)

txsQuery3 :: QueryTxs
txsQuery3 = SELECTTxs WHERE (TxGrade :>= Core.B)

txsQuery4 :: QueryTxs
txsQuery4 = SELECTTxs WHERE (TxSubjectIdEq 2 :& TxGrade :>= Core.B)

txsQuery5 :: QueryTxs
txsQuery5 = SELECTTxs WHERE (TxSubjectIdEq 2 :& TxGrade :>= Core.A)



simpleTxDB :: [PrivateTx]
simpleTxDB = fmap (uncurry mkPrivateTx) (zip ['a'..'j'] ['k'..'t'])

type StudentKey = Char
type EducatorKey = Char

-- | create a simple private transaction
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
   payload2 = EducatorTx { _ptxEducatorMsg  = GradeCourse Core.B }

-- | create key pair from seed
mkKeyPair :: Char -> (PublicKey, SecretKey)
mkKeyPair seed =
  let (CryptoPassed x) = Ed25519.secretKey (C.replicate 32 seed)
  in (AbstractPK (Ed25519.toPublic x), AbstractSK x)

