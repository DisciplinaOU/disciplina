module Disciplina.DB.DSL.Interpret
              ( runQuery
              )
              where

import Universum
import Disciplina.DB.Class (MonadDBRead)
import Disciplina.DB.DSL.Types (QueryTx(..), QueryTxs(..), WHERE (..)
                               ,TxIdEq (..), TxGrade(..), QueryObj(..)
                               ,TxsFilterExpr(..), ObjHashEq(..))
import Disciplina.Educator.Txs (PrivateTxId(..), PrivateTx(..))
import Disciplina.Crypto (Hash)
import qualified Disciplina.Core as Core (Grade(..), SubjectId(..))

-- | Query type determine query return type
class RunQuery a b | a -> b where
  runQuery :: (MonadDBRead m) => a -> m b

-- | TODO, implement real interpreters, these are just stubs
runTxQuery :: (MonadDBRead m) => QueryTx -> m (Maybe PrivateTx)
runTxQuery (SELECTTx _ (TxIdEq (a :: PrivateTxId))) = return Nothing

instance RunQuery QueryTx (Maybe PrivateTx) where
  runQuery = runTxQuery

runObjQuery (SELECTObj _ (ObjHashEq (a :: Hash ()))) = return Nothing

instance RunQuery QueryTxs [PrivateTx] where
  runQuery = runTxsQuery

runTxsQuery (SELECTTxs _ (TxSubjectIdEq (a :: Int))) = return []
runTxsQuery (SELECTTxs _ (TxGradeEq (a :: Core.Grade))) = return []
runTxsQuery (SELECTTxs _ ((:>=) _ (a :: Core.Grade))) = return []
runTxsQuery (SELECTTxs _ ((:~>) a b)) = return []
runTxsQuery (SELECTTxs _ ((:&) a b)) = do
                  resA <- runTxsQuery (SELECTTxs WHERE a)
                  resB <- runTxsQuery (SELECTTxs WHERE b)
                  return []
runTxsQuery (SELECTTxs _ ((:||) a b)) = do
                  resA <- runTxsQuery (SELECTTxs WHERE a)
                  resB <- runTxsQuery (SELECTTxs WHERE b)
                  return []

instance RunQuery QueryObj (Maybe Int) where
  runQuery = runObjQuery


{-- query construction

-- Precise tx queries:
-- given <tx_hash>, I want to be able to get { <tx> | hash(<tx>) = <tx_hash> }
q1 :: PrivateTxId -> QueryTx
q1 txId = SELECTTx WHERE (TxIdEq txId)

-- Precise object queries:
-- given <obj_hash>, I want to be able to get { <obj> | hash(<obj>) = <obj_hash> }
q2 :: Hash a -> QueryObj a
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
                           :& SubjectNode subjId :~> TxSubjectNode)

runq1 :: IO (Maybe PrivateTx)
runq1 = runQuery (q1 (undefined :: PrivateTxId))

runq2 :: IO (Maybe Int)
runq2 = runQuery (q2 (undefined :: Hash a))

runq3 :: IO [PrivateTx]
runq3 = runQuery q3

runq4 :: IO [PrivateTx]
runq4 = runQuery (q4 1)

--}
