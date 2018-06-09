module Disciplina.DB.DSL.Interpret.IO
       ( runQuery
       )
       where

import Universum

import Data.List (intersect, union)

import qualified Disciplina.Core as Core (Grade (..), SubjectId (..))
import Disciplina.DB.DSL.Class (MonadSearchTxObj (..), Obj, ObjHashEq (..), QueryObj (..),
                                QueryTx (..), QueryTxs (..), RunQuery (..), TxIdEq (..),
                                TxsFilterExpr (..), WHERE (..))
import Disciplina.Educator.Txs (PrivateTx (..), PrivateTxId)

-- | TODO, we should have a proper monad here, not IO
instance MonadSearchTxObj IO where
    runTxQuery = runIOTxQuery
    runTxsQuery = runIOTxsQuery
    runObjQuery = runIOObjQuery

-- | TODO, implement real interpreters, these are just stubs
runIOTxQuery :: QueryTx -> IO (Maybe PrivateTx)
runIOTxQuery (SELECTTx _ (TxIdEq (_ :: PrivateTxId))) =
    return Nothing

runIOTxsQuery :: QueryTxs -> IO [PrivateTx]
runIOTxsQuery (SELECTTxs _ (TxHasSubjectId (_ :: Core.SubjectId))) =
    return []
runIOTxsQuery (SELECTTxs _ (TxGradeEq (_ :: Core.Grade))) =
    return []
runIOTxsQuery (SELECTTxs _ ((:>=) _ (_ :: Core.Grade))) =
    return []
runIOTxsQuery (SELECTTxs _ (TxHasDescendantOfSubjectId _)) =
    return []
runIOTxsQuery (SELECTTxs _ ((:&) a b)) = do
    resA <- runIOTxsQuery (SELECTTxs WHERE a)
    resB <- runIOTxsQuery (SELECTTxs WHERE b)
    return (resA `intersect` resB)
runIOTxsQuery (SELECTTxs _ ((:||) a b)) = do
    resA <- runIOTxsQuery (SELECTTxs WHERE a)
    resB <- runIOTxsQuery (SELECTTxs WHERE b)
    return (resA `union` resB)


runIOObjQuery :: QueryObj -> IO (Maybe Obj)
runIOObjQuery (SELECTObj _ (ObjHashEq _)) =
    return Nothing
