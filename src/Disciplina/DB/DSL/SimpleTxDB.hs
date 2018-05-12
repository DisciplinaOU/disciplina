module Disciplina.DB.DSL.SimpleTxDB
       (runSimpleTxDBQuery
       ) where

import Universum

import Codec.Serialise (Serialise(..))

import Disciplina.DB.DSL.Types (QueryTx(..), QueryTxs(..), WHERE(..)
                               ,TxIdEq(..), TxsFilterExpr(..))
import Disciplina.Educator.Txs (PrivateTxId, PrivateTx(..), EducatorTxMsg(..)
                               ,StudentTxMsg(..), PrivateTxPayload(..))
import Disciplina.Crypto (hash)
import Disciplina.Core (Address(..), CourseId(..), Grade(..)
                       , hasPathFromTo, activityTypeGraphIndexed)
import Disciplina.DB.DSL.Interpret (MonadSearchTxObj(..), RunQuery(..))
import Data.List (union, intersect)


-- | Simple transaction database
newtype SimpleTxDB a = SimpleTxDB
   { getSimpleTxDB :: Reader [PrivateTx] a
   } deriving (Functor, Applicative, Monad, MonadReader [PrivateTx])

instance MonadSearchTxObj SimpleTxDB where
  runTxQuery = evalSimpleTxQuery
  runTxsQuery = evalSimpleTxsQuery
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


-- | Evaluator for query: find Tx in db with hash == h
evalSimpleTxQuery :: QueryTx -> SimpleTxDB (Maybe PrivateTx)
evalSimpleTxQuery (SELECTTx _ (TxIdEq (h :: PrivateTxId))) =
  safeHead.filter ((==h).hash) <$> ask

-- | Evaluator for query: find Txs in db with SubjectId == a
evalSimpleTxsQuery :: QueryTxs -> SimpleTxDB [PrivateTx]
evalSimpleTxsQuery (SELECTTxs _ (TxSubjectIdEq sId)) =
  filter ((==sId).ciSubject._ptxCourseId) <$> ask

-- | Evaluator for query: find Txs in db with grade == g
evalSimpleTxsQuery (SELECTTxs _ (TxGradeEq grade)) =
  filter ((== Just grade).getGrade._ptxPayload) <$> ask
  where getGrade (EducatorTx (GradeCourse g)) = Just g
        getGrade _ = Nothing

-- | Evaluator for query: find Txs in db with grade >= g
evalSimpleTxsQuery (SELECTTxs _ (_ :>= grade)) = do
  filter ((>= Just grade).getGrade._ptxPayload) <$> ask
  where getGrade (EducatorTx (GradeCourse g)) = Just g
        getGrade _ = Nothing

-- | Evaluator for AND query
evalSimpleTxsQuery (SELECTTxs _ (a :& b)) =
   intersect <$> runQuery (SELECTTxs WHERE a) <*> runQuery (SELECTTxs WHERE b)

-- | Evaluator for OR query
evalSimpleTxsQuery (SELECTTxs _ (a :|| b)) =
   union <$> runQuery (SELECTTxs WHERE a) <*> runQuery (SELECTTxs WHERE b)

-- | Evaluator for query: find all txs with subjectId which is descendant of sId
evalSimpleTxsQuery (SELECTTxs _ (TxSubjectIsDescendantOf sId)) =
  filter (isDescendantOf sId . ciSubject ._ptxCourseId) <$> ask
  where isDescendantOf x y = hasPathFromTo activityTypeGraphIndexed x y

-- | Run query in SimpleTxDB
runSimpleTxDBQuery :: RunQuery a b => [PrivateTx] -> a -> b
runSimpleTxDBQuery db query = runReader (getSimpleTxDB . runQuery $ query) db
