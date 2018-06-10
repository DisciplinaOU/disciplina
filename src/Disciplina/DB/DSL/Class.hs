{-# LANGUAGE TypeOperators #-}

module Disciplina.DB.DSL.Class
       ( MonadSearchTxObj (..)
       , TxsFilterExpr (..)
       , TxGrade (..)
       , TxIdEq (..)
       , RunQuery (..)
       , Obj
       , ObjHashEq (..)
       , QueryTx (..)
       , QueryTxs (..)
       , QueryObj (..)
       , WHERE (..)
       ) where

import Universum

import Disciplina.Educator.Txs (PrivateTxId, PrivateTx(..))
import Disciplina.Crypto (Hash)

import qualified Disciplina.Core as Core (Grade, SubjectId)
import qualified Data.ByteString.Lazy as LBS

data WHERE = WHERE

data TxIdEq = TxIdEq PrivateTxId

data TxGrade = TxGrade

data TxsFilterExpr = TxHasSubjectId Core.SubjectId
                   | TxGradeEq Core.Grade
                   | (:>=) TxGrade Core.Grade
                   | (:&) TxsFilterExpr TxsFilterExpr
                   | (:||) TxsFilterExpr TxsFilterExpr
                   | TxHasDescendantOfSubjectId Core.SubjectId

infixr 4 :>=
infixr 4 :||
infixr 3 :&

-- | TODO, use real Obj type
type Obj = LBS.ByteString

data ObjHashEq = ObjHashEq (Hash Obj)

-- | DSL type is split into different data types
-- so we can dispatch over interpreters
data QueryTx = SELECTTx WHERE TxIdEq

data QueryObj = SELECTObj WHERE ObjHashEq

data QueryTxs = SELECTTxs WHERE TxsFilterExpr

class (Monad m) => MonadSearchTxObj m where
    runTxQuery :: QueryTx -> m (Maybe PrivateTx)
    runTxsQuery :: QueryTxs -> m [PrivateTx]
    runObjQuery :: QueryObj -> m (Maybe Obj)

-- | Query type determine query return type
class RunQuery a b | a -> b where
    runQuery :: (MonadSearchTxObj m) => a -> m b

instance RunQuery QueryTx (Maybe PrivateTx) where
    runQuery = runTxQuery

instance RunQuery QueryObj (Maybe Obj) where
    runQuery = runObjQuery

instance RunQuery QueryTxs [PrivateTx] where
    runQuery = runTxsQuery
