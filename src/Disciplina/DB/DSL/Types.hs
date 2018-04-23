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

-- todo, what is the correct type for TxSubjectIdEq?
data TxsFilterExpr = TxSubjectIdEq Int
                   | TxGradeEq Core.Grade
                   | (:>=) TxGrade Core.Grade
                   | (:&) TxsFilterExpr TxsFilterExpr
                   | (:~>) NodeWithProp NodeWithProp

infixr 4 :>=
infixr 3 :&
infixr 4 :~>

-- todo, use correct object hash type
data ObjHashEq a = ObjHashEq (Hash a)

-- DSL type is split into different data types
-- so we can dispatch over interpreters
data QueryTx = SELECTTx WHERE TxIdEq

data QueryObj a = SELECTObj WHERE (ObjHashEq a)

data QueryTxs = SELECTTxs WHERE TxsFilterExpr
