module Disciplina.DB.DSL.Interpret.SimpleTxDB
       (runSimpleTxDBQuery
       ) where

import Universum

import Control.Lens (filtered, makePrisms, traversed)

import Data.List (intersect, union)
import Disciplina.Crypto (hash)
import Disciplina.DB.DSL.Class (MonadSearchTxObj (..), Obj, ObjHashEq (..), QueryObj (..),
                                QueryTx (..), QueryTxs (..), RunQuery (..), TxIdEq (..),
                                TxsFilterExpr (..), WHERE (..))
import Disciplina.Educator (EducatorTxMsg (..), PrivateTx (..), PrivateTxId, PrivateTxPayload (..))

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

-- | Evaluator for query: find Tx in db with hash == h
evalSimpleTxQuery :: QueryTx -> SimpleTxDB (Maybe PrivateTx)
evalSimpleTxQuery (SELECTTx _ (TxIdEq (h :: PrivateTxId))) = do
    db <- ask
    return $ db ^? traversed . _SSTx . filtered (((h==).hash))

-- | Evaluator for query: find Obj in db with obj hash == h
evalSimpleObjQuery :: QueryObj -> SimpleTxDB (Maybe Obj)
evalSimpleObjQuery (SELECTObj _ (ObjHashEq h)) = do
    db <- ask
    return $ db ^? traversed . _SSObj . filtered (((h==).hash))

-- | Evaluator for query: find Txs in db with SubjectId == a
evalSimpleTxsQuery :: QueryTxs -> SimpleTxDB [PrivateTx]
evalSimpleTxsQuery (SELECTTxs _ (TxSubjectIdEq _)) = do
    return []
    -- TODO: restore actual logic after introducing multi-subjects courses.
    -- db <- ask
    -- return $ db ^.. traversed
    --     . _SSTx
    --     . filtered ((==sId) . ciSubject . _ptxCourseId)

-- | Evaluator for query: find Txs in db with grade == g
evalSimpleTxsQuery (SELECTTxs _ (TxGradeEq grade)) = do
    db <- ask
    return $ db ^.. traversed
        . _SSTx
        . filtered ((== Just grade).getGrade._ptxPayload)
  where getGrade (EducatorTx (GradeCourse g)) = Just g
        getGrade _                            = Nothing

-- | Evaluator for query: find Txs in db with grade >= g
evalSimpleTxsQuery (SELECTTxs _ (_ :>= grade)) = do
    db <- ask
    return $ db ^.. traversed
        . _SSTx
        . filtered ((>= Just grade).getGrade._ptxPayload)
  where getGrade (EducatorTx (GradeCourse g)) = Just g
        getGrade _                            = Nothing

-- | Evaluator for AND query
evalSimpleTxsQuery (SELECTTxs _ (a :& b)) =
    intersect <$> runQuery (SELECTTxs WHERE a) <*> runQuery (SELECTTxs WHERE b)

-- | Evaluator for OR query
evalSimpleTxsQuery (SELECTTxs _ (a :|| b)) =
    union <$> runQuery (SELECTTxs WHERE a) <*> runQuery (SELECTTxs WHERE b)

-- | Evaluator for query: find all txs with subjectId which is descendant of sId
evalSimpleTxsQuery (SELECTTxs _ (TxSubjectIsDescendantOf _)) = do
    return []
  -- TODO: restore actual logic after introducing multi-subject courses.
  -- db <- ask
  --   return $ db ^.. traversed
  --       . _SSTx
  --       . filtered (isDescendantOf sId . ciSubject . _ptxCourseId)
  -- where isDescendantOf x y = hasPathFromTo activityTypeGraphIndexed x y

-- | Run query in SimpleTxDB
runSimpleTxDBQuery :: RunQuery a b => [PrivateTx] -> [Obj] -> a -> b
runSimpleTxDBQuery dbTx dbObj query = runReader (getSimpleTxDB . runQuery $ query) mkDb
  where mkDb = fmap SSTx dbTx <> fmap SSObj dbObj
