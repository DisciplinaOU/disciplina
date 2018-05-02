module Disciplina.DB.DSL.Interpret
              ( runQuery
              , RunQuery (..)
              )
              where

import Universum
import Disciplina.DB.DSL.Types (QueryTx(..), QueryTxs(..), WHERE (..)
                               ,TxIdEq (..), TxGrade(..), QueryObj(..)
                               ,TxsFilterExpr(..), ObjHashEq(..))
import Disciplina.Educator.Txs (PrivateTxId(..), PrivateTx(..))
import Disciplina.Crypto (Hash)
import qualified Disciplina.Core as Core (Grade(..), SubjectId(..))

class (Monad m) => MonadSearchTxObj m where
  runTxQuery :: QueryTx -> m (Maybe PrivateTx)
  runTxsQuery :: QueryTxs -> m [PrivateTx]
  runObjQuery :: QueryObj -> m (Maybe Int)

instance MonadSearchTxObj IO where
  runTxQuery = runIOTxQuery
  runTxsQuery = runIOTxsQuery
  runObjQuery = runIOObjQuery

-- | Query type determine query return type
class RunQuery a b | a -> b where
  runQuery :: (MonadSearchTxObj m) => a -> m b

-- | TODO, implement real interpreters, these are just stubs
runIOTxQuery :: QueryTx -> IO (Maybe PrivateTx)
runIOTxQuery (SELECTTx _ (TxIdEq (a :: PrivateTxId))) = return Nothing

instance RunQuery QueryTx (Maybe PrivateTx) where
  runQuery = runTxQuery

instance RunQuery QueryTxs [PrivateTx] where
  runQuery = runTxsQuery

runIOTxsQuery :: QueryTxs -> IO [PrivateTx]
runIOTxsQuery (SELECTTxs _ (TxSubjectIdEq (a :: Int))) = return []
runIOTxsQuery (SELECTTxs _ (TxGradeEq (a :: Core.Grade))) = return []
runIOTxsQuery (SELECTTxs _ ((:>=) _ (a :: Core.Grade))) = return []
runIOTxsQuery (SELECTTxs _ (TxSubjectIsDescendantOf a)) = return []
runIOTxsQuery (SELECTTxs _ ((:&) a b)) = do
                  resA <- runIOTxsQuery (SELECTTxs WHERE a)
                  resB <- runIOTxsQuery (SELECTTxs WHERE b)
                  return []
runIOTxsQuery (SELECTTxs _ ((:||) a b)) = do
                  resA <- runIOTxsQuery (SELECTTxs WHERE a)
                  resB <- runIOTxsQuery (SELECTTxs WHERE b)
                  return []

instance RunQuery QueryObj (Maybe Int) where
  runQuery = runObjQuery

runIOObjQuery :: QueryObj -> IO (Maybe Int)
runIOObjQuery (SELECTObj _ (ObjHashEq (a :: Hash ()))) = return Nothing


{-- query construction

-- Precise tx queries:
-- given <tx_hash>, I want to be able to get { <tx> | hash(<tx>) = <tx_hash> }
q1 :: PrivateTxId -> QueryTx
q1 txId = SELECTTx WHERE (TxIdEq txId)

-- Precise object queries:
-- given <obj_hash>, I want to be able to get { <obj> | hash(<obj>) = <obj_hash> }
q2 :: Hash () -> QueryObj
q2 objHash = SELECTObj WHERE (ObjHashEq objHash)

-- Range queries:
-- given <ordered_tx_param>,
-- I want to be able to get { [<tx>] | P(<ordered_tx_param>) }, where P is a system of (in)equalities
q3 :: QueryTxs
q3 = SELECTTxs WHERE (TxSubjectIdEq 1 :& TxGrade :>= Core.B)

-- Subject queries:
-- given <subject_name>,
-- I want to be able to get { [<tx>] | subj(<tx>) = S, <subject_name> ~> S },
-- where "A ~> B" denotes "there's a path in ATG from A to B"
q4 :: Core.SubjectId -> QueryTxs
q4 subjId = SELECTTxs WHERE (TxSubjectIdEq 1
                           :& TxGradeEq Core.B
                           :& TxSubjectIsDescendantOf subjId)

runq1 :: (MonadSearchTxObj m) => m (Maybe PrivateTx)
runq1 = runQuery (q1 (undefined :: PrivateTxId))

runq2 :: (MonadSearchTxObj m) => m (Maybe Int)
runq2 = runQuery (q2 (undefined :: Hash a))

runq3 :: (MonadSearchTxObj m) => m [PrivateTx]
runq3 = runQuery q3

runq4 :: (MonadSearchTxObj m) => m [PrivateTx]
runq4 = runQuery (q4 1)

--}
