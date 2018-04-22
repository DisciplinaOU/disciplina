{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Disciplina.DB.DSL.Types
       ( Query (..)
        ,WHERE(..)
        ,Tx(..)
        ,Txs(..)
        ,Obj(..)
        ,FilterEntity(..)
        ,FilterExpr(..)
        ,NodeWithProp(..)
       ) where

import Universum
import qualified Disciplina.Core as Core (Grade(..), SubjectId(..))



class FilterEntity a b where
  data FilterLHS a b :: * -- LHS of equation
  type FilterRHS a b :: * -- RHS of equation


data Tx = Tx
data TxHash

instance FilterEntity Tx TxHash where
  data FilterLHS Tx TxHash = TxHash
  type FilterRHS Tx TxHash = Text


data Txs = Txs
data TxSubjectId
data TxGrade

instance FilterEntity Txs TxSubjectId where
  data FilterLHS Txs TxSubjectId = TxSubjectId
  type FilterRHS Txs TxSubjectId = Core.SubjectId

instance FilterEntity Txs TxGrade  where
  data FilterLHS Txs TxGrade = TxGrade
  type FilterRHS Txs TxGrade = Core.Grade


data Obj = Obj
data ObjHash
instance FilterEntity Obj ObjHash where
  data FilterLHS Obj ObjHash = ObjHash
  type FilterRHS Obj ObjHash = Text



data NodeWithProp = SubjectNode Core.SubjectId
                  | TxSubjectNode

data WHERE = WHERE


data FilterExpr a b where
   (:==) :: FilterLHS a b -> FilterRHS a b -> FilterExpr a b
   (:>=) :: FilterLHS a b -> FilterRHS a b -> FilterExpr a b
   (:&)  :: FilterExpr a b -> FilterExpr c d -> FilterExpr e f
   (:~>) :: NodeWithProp -> NodeWithProp -> FilterExpr a b

infixr 4 :>=
infixr 3 :&
infixr 4 :==
infixr 4 :~>

data Query a b = SELECT a WHERE (FilterExpr a b)


-- query construction

-- Precise tx queries:
-- given <tx_hash>, I want to be able to get { <tx> | hash(<tx>) = <tx_hash> }
--q1 txHash = SELECT Tx WHERE (TxHash :== Hash "abc")
q1 txHash = SELECT Tx WHERE (TxHash :== txHash)

-- Precise object queries:
-- given <obj_hash>, I want to be able to get { <obj> | hash(<obj>) = <obj_hash> }
q2 objHash = SELECT Obj WHERE (ObjHash :== objHash)

-- Range queries:
-- given <ordered_tx_param>,
-- I want to be able to get { [<tx>] | P(<ordered_tx_param>) }, where P is a system of (in)equalities
q3 txParams = SELECT Txs WHERE (TxSubjectId :== 1 :& TxGrade :>= Core.B)

-- Subject queries:
-- given <subject_name>,
-- I want to be able to get { [<tx>] | subj(<tx>) = S, <subject_name> ~> S },
-- where "A ~> B" denotes "there's a path in ATG from A to B"
q4 txParams subjId = SELECT Txs WHERE (TxSubjectId :== 1
                                    :& TxGrade :>= Core.B
                                    :& SubjectNode subjId :~> TxSubjectNode)

-- Range queries and subject queries should be able to be combined by OR and AND operators.
-- NOT operator isn't going to be supported (most likely).
