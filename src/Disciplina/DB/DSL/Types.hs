{-# LANGUAGE TypeOperators #-}

module Disciplina.DB.DSL.Types
       ( Query (..)
        ,(:==)(..)
        ,(:>=)(..)
        ,(:&)(..)
       ) where

import Universum
import Disciplina.Core (Grade(..))

data Entity = Tx
            | Txs
            | Obj

data a :>= b = a :>= b
infixr 5 :>= -- todo, chose infixr

data a :& b = a :& b
infixr 4 :& -- todo, chose infixr

data a :== b = a :== b
infixr 3 :== -- todo, chose infixr

data Filters a = SubjectId a
               | Done

data Grade = Grade

data WHERE = WHERE

data WHEREStm a = HashEq Text
                | FilterCond a

data Query a = SELECT Entity WHERE (WHEREStm a)


-- query construction

-- Precise tx queries:
-- given <tx_hash>, I want to be able to get { <tx> | hash(<tx>) = <tx_hash> }
q1 txHash = SELECT Tx WHERE (HashEq txHash)

-- Precise object queries:
-- given <obj_hash>, I want to be able to get { <obj> | hash(<obj>) = <obj_hash> }
q2 objHash = SELECT Obj WHERE (HashEq objHash)

-- Range queries:
-- given <ordered_tx_param>,
-- I want to be able to get { [<tx>] | P(<ordered_tx_param>) }, where P is a system of (in)equalities
q3 txParams = SELECT Txs WHERE (FilterCond (SubjectId :== 1 :& Grade :>= B :& Grade :== C))

-- Subject queries:
-- given <subject_name>,
-- I want to be able to get { [<tx>] | subj(<tx>) = S, <subject_name> ~> S },
-- where "A ~> B" denotes "there's a path in ATG from A to B"
q4 = undefined

-- Range queries and subject queries should be able to be combined by OR and AND operators.
-- NOT operator isn't going to be supported (most likely).


