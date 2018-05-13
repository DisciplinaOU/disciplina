module Disciplina.DB.DSL.Interpret.SimpleTxDB
       (runSimpleTxDBQuery
       ) where

import Universum

import Codec.Serialise (Serialise(..))
import Control.Lens (traversed, filtered, makePrisms)

import Disciplina.DB.DSL.Class (QueryTx(..), QueryTxs(..), ObjHashEq(..), Obj
                               ,QueryObj(..), MonadSearchTxObj(..), RunQuery(..)
                               ,WHERE(..), TxIdEq(..), TxsFilterExpr(..))
import Disciplina.Educator.Txs (PrivateTxId, PrivateTx(..), EducatorTxMsg(..)
                               ,StudentTxMsg(..), PrivateTxPayload(..))
import Disciplina.Crypto (hash)
import Disciplina.Core (Address(..), CourseId(..), Grade(..)
                       , hasPathFromTo, activityTypeGraphIndexed)
import Data.List (union, intersect)

data SimpleObj = SSTx { _sstorePTx :: PrivateTx}
               | SSObj { _sstoreObj :: Obj }

makePrisms ''SimpleObj

-- | Simple transaction database
newtype SimpleTxDB a = SimpleTxDB
   { getSimpleTxDB :: Reader [SimpleObj] a
   } deriving (Functor, Applicative, Monad, MonadReader [SimpleObj])

instance MonadSearchTxObj SimpleTxDB where
  runTxQuery = evalSimpleTxQuery
  runTxsQuery = evalSimpleTxsQuery
  runObjQuery = evalSimpleObjQuery

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
evalSimpleTxQuery (SELECTTx _ (TxIdEq (h :: PrivateTxId))) = do
  db <- ask
  return $ db ^?  traversed . _SSTx . filtered (((h==).hash))

-- | Evaluator for query: find Tx in db with obj hash == h
evalSimpleObjQuery :: QueryObj -> SimpleTxDB (Maybe Obj)
evalSimpleObjQuery (SELECTObj _ (ObjHashEq h)) = do
  db <- ask
  return $ db ^?  traversed . _SSObj . filtered (((h==).hash))

-- | Evaluator for query: find Txs in db with SubjectId == a
evalSimpleTxsQuery :: QueryTxs -> SimpleTxDB [PrivateTx]
evalSimpleTxsQuery (SELECTTxs _ (TxSubjectIdEq sId)) = do
  db <- ask
  return $ db ^..  traversed
              . _SSTx
              . filtered ((==sId).ciSubject._ptxCourseId)

-- | Evaluator for query: find Txs in db with grade == g
evalSimpleTxsQuery (SELECTTxs _ (TxGradeEq grade)) = do
  db <- ask
  return $ db ^..  traversed
              . _SSTx
              . filtered ((== Just grade).getGrade._ptxPayload)
  where getGrade (EducatorTx (GradeCourse g)) = Just g
        getGrade _ = Nothing

-- | Evaluator for query: find Txs in db with grade >= g
evalSimpleTxsQuery (SELECTTxs _ (_ :>= grade)) = do
  db <- ask
  return $ db ^..  traversed
              . _SSTx
              . filtered ((>= Just grade).getGrade._ptxPayload)
  where getGrade (EducatorTx (GradeCourse g)) = Just g
        getGrade _ = Nothing

-- | Evaluator for AND query
evalSimpleTxsQuery (SELECTTxs _ (a :& b)) =
   intersect <$> runQuery (SELECTTxs WHERE a) <*> runQuery (SELECTTxs WHERE b)

-- | Evaluator for OR query
evalSimpleTxsQuery (SELECTTxs _ (a :|| b)) =
   union <$> runQuery (SELECTTxs WHERE a) <*> runQuery (SELECTTxs WHERE b)

-- | Evaluator for query: find all txs with subjectId which is descendant of sId
evalSimpleTxsQuery (SELECTTxs _ (TxSubjectIsDescendantOf sId)) = do
  db <- ask
  return $ db ^..  traversed
              . _SSTx
              . filtered (isDescendantOf sId . ciSubject ._ptxCourseId)
  where isDescendantOf x y = hasPathFromTo activityTypeGraphIndexed x y

-- | Run query in SimpleTxDB
runSimpleTxDBQuery :: RunQuery a b => [PrivateTx] -> a -> b
runSimpleTxDBQuery db query = runReader (getSimpleTxDB . runQuery $ query) mkDb
 where mkDb = fmap SSTx db
