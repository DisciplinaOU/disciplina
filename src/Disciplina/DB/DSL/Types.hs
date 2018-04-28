{-# LANGUAGE TypeOperators #-}

module Disciplina.DB.DSL.Types
        ( WHERE (..)
        , TxIdEq (..)
        , TxGrade (..)
        , NodeWithProp (..)
        , TxsFilterExpr (..)
        , ObjHashEq (..)
        , QueryTx (..)
        , QueryTxs (..)
        , QueryObj (..)
        )
        where

import Universum
import Disciplina.Educator.Txs (PrivateTxId)
import Disciplina.Crypto (Hash)
import qualified Disciplina.Core as Core (Grade, SubjectId)

data WHERE = WHERE

data TxIdEq = TxIdEq PrivateTxId

data TxGrade = TxGrade

data NodeWithProp = SubjectNode Core.SubjectId
                  | TxSubjectNode

-- | TODO, what is the correct type for TxSubjectIdEq?
data TxsFilterExpr = TxSubjectIdEq Int
                   | TxGradeEq Core.Grade
                   | (:>=) TxGrade Core.Grade
                   | (:&) TxsFilterExpr TxsFilterExpr
                   | (:||) TxsFilterExpr TxsFilterExpr
                   | (:~>) NodeWithProp NodeWithProp

infixr 4 :>=
infixr 4 :||
infixr 3 :&
infixr 4 :~>

-- | TODO, use correct object hash type
type Hash' = Hash ()

data ObjHashEq = ObjHashEq Hash'

-- DSL type is split into different data types
-- so we can dispatch over interpreters
data QueryTx = SELECTTx WHERE TxIdEq

data QueryObj = SELECTObj WHERE ObjHashEq

data QueryTxs = SELECTTxs WHERE TxsFilterExpr
