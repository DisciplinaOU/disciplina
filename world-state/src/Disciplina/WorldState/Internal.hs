
module Disciplina.WorldState.Internal where

import Universum

import Control.Lens
import Control.Monad.Writer.Strict

import Disciplina.Accounts
import Disciplina.WorldState.BlakeHash

import Pos.Crypto.Hashing

import qualified Data.Tree.AVL as AVL

data Identity = Identity Int

data WorldState
  = WorldState
    { _wsAccounts       :: AVL.Map Hash Identity Account
    , _wsPublications   :: AVL.Map Hash Identity Publication
    , _wsSpecalizations :: AVL.Map Hash Identity DAG
    , _wsStorage        :: AVL.Map Hash Identity Storage
    }

type DAG         = ()
type Publication = Hash
type Storage     = ()

data TreeName
    = TNAccounts
    | TNPulications
    | TNSpecializations
    | TNStorage

newWorldState
    :: MonadWriter proof m
    => m WorldState
newWorldState =
    WorldState
        <$> AVL.empty
        <*> AVL.empty
        <*> AVL.empty

makeLenses ''WorldState
