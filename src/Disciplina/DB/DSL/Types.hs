{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Disciplina.DB.DSL.Types
       ( Query (..)
        ,WHERE(..)
        ,Entity(..)
        ,FilterEntity(..)
        ,FilterExpr(..)
        ,NodeWithProp(..)
       ) where

import Universum
import qualified Disciplina.Core as Core (Grade(..))


data Entity = Tx
            | Txs
            | Obj

data FilterEntity = TxHash
                  | ObjHash
                  | Hash Text
                  | TxSubjectId
                  | SubjectId Int
                  | TxGrade
                  | Grade Core.Grade


data NodeWithProp = NodeWithSubjectName Text
                  | NodeWithTxSubjectName

data WHERE = WHERE

data FilterExpr where
   (:==) :: FilterEntity -> FilterEntity -> FilterExpr
   (:>=) :: FilterEntity -> FilterEntity -> FilterExpr
   (:&)  :: FilterExpr -> FilterExpr -> FilterExpr
   (:~>) :: NodeWithProp -> NodeWithProp -> FilterExpr

infixr 4 :>=
infixr 3 :&
infixr 4 :==
infixr 4 :~>


data Query = SELECT Entity WHERE FilterExpr

-- query construction

-- Precise tx queries:
-- given <tx_hash>, I want to be able to get { <tx> | hash(<tx>) = <tx_hash> }
q1 txHash = SELECT Tx WHERE (TxHash :== Hash "abc")

-- Precise object queries:
-- given <obj_hash>, I want to be able to get { <obj> | hash(<obj>) = <obj_hash> }
q2 objHash = SELECT Obj WHERE (ObjHash :== Hash objHash)

-- Range queries:
-- given <ordered_tx_param>,
-- I want to be able to get { [<tx>] | P(<ordered_tx_param>) }, where P is a system of (in)equalities
q3 txParams = SELECT Txs WHERE (TxSubjectId :== SubjectId 1 :& TxGrade :>= Grade Core.B)

-- Subject queries:
-- given <subject_name>,
-- I want to be able to get { [<tx>] | subj(<tx>) = S, <subject_name> ~> S },
-- where "A ~> B" denotes "there's a path in ATG from A to B"
q4 txParams subjName = SELECT Txs WHERE (TxSubjectId :== SubjectId 1
                                      :& TxGrade :>= Grade Core.B
                                      :& NodeWithSubjectName subjName :~> NodeWithTxSubjectName)

-- Range queries and subject queries should be able to be combined by OR and AND operators.
-- NOT operator isn't going to be supported (most likely).
