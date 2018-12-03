module Dscp.Educator.DB.DSL.Interpret.IO
       (
       )
       where

import Data.List (intersect, union)

import qualified Dscp.Core as Core (Grade (..), Subject (..))
import Dscp.Core.Foundation (PrivateTx (..))
import Dscp.Educator.DB.DSL.Class (MonadSearchTxObj (..), Obj, ObjHashEq (..), QueryObj (..),
                                   QueryTx (..), QueryTxs (..), TxIdEq (..), TxsFilterExpr (..),
                                   WHERE (..))
import Dscp.Util (HasId (Id))

-- | TODO, we should have a proper monad here, not IO
instance MonadSearchTxObj IO where
    runTxQuery = runIOTxQuery
    runTxsQuery = runIOTxsQuery
    runObjQuery = runIOObjQuery

-- | TODO, implement real interpreters, these are just stubs
runIOTxQuery :: QueryTx -> IO (Maybe PrivateTx)
runIOTxQuery (SELECTTx _ (TxIdEq (_ :: Id PrivateTx))) =
    return Nothing

runIOTxsQuery :: QueryTxs -> IO [PrivateTx]
runIOTxsQuery (SELECTTxs _ (TxHasSubjectId (_ :: Id Core.Subject))) =
    return []
runIOTxsQuery (SELECTTxs _ ((:==) _ (_ :: Core.Grade))) =
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
