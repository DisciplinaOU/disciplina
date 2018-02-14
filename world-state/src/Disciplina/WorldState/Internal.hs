
{-# language DeriveAnyClass #-}

module Disciplina.WorldState.Internal where

import Universum

import Control.Lens
import Control.Monad.Writer.Strict

import Data.Binary

import Disciplina.Accounts
import Disciplina.WorldState.BlakeHash

import qualified Data.Tree.AVL as AVL

data Entity = Entity Int
    deriving (Show, Eq, Ord, Bounded, Generic, Binary)

data WorldState
  = WorldState
    { _wsAccounts       :: AVL.Map Hash Entity (Account Hash)
    , _wsPublications   :: AVL.Map Hash Entity Publication
    , _wsSpecalizations :: AVL.Map Hash Entity DAG
    , _wsStorage        :: AVL.Map Hash Entity Storage
    }

type DAG         = ()
type Publication = Hash
type Storage     = ()

data TreeName
    = TNAccounts
    | TNPulications
    | TNSpecializations
    | TNStorage

emptyWorldState :: WorldState
emptyWorldState =
    WorldState AVL.empty AVL.empty AVL.empty AVL.empty

makeLenses ''WorldState
