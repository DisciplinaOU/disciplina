module Disciplina.DB.DSL.Interpret.IO
       ( runQuery
       )
       where

import Universum
import Disciplina.DB.DSL.Class (QueryTx(..), QueryTxs(..), WHERE (..)
                               ,TxIdEq (..), TxGrade(..), Obj, QueryObj(..)
                               ,TxsFilterExpr(..), ObjHashEq(..)
                               ,MonadSearchTxObj(..),RunQuery(..))
import Disciplina.Educator.Txs (PrivateTxId(..), PrivateTx(..))
import Disciplina.Crypto (Hash)
import qualified Disciplina.Core as Core (Grade(..), SubjectId(..))

-- | TODO, we should have a proper monad here, not IO
instance MonadSearchTxObj IO where
  runTxQuery = runIOTxQuery
  runTxsQuery = runIOTxsQuery
  runObjQuery = runIOObjQuery

-- | TODO, implement real interpreters, these are just stubs
runIOTxQuery :: QueryTx -> IO (Maybe PrivateTx)
runIOTxQuery (SELECTTx _ (TxIdEq (a :: PrivateTxId))) = return Nothing

runIOTxsQuery :: QueryTxs -> IO [PrivateTx]
runIOTxsQuery (SELECTTxs _ (TxSubjectIdEq (a :: Core.SubjectId))) = return []
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


runIOObjQuery :: QueryObj -> IO (Maybe Obj)
runIOObjQuery (SELECTObj _ (ObjHashEq a)) = return Nothing
