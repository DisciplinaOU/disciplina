
module Disciplina.WorldState.Internal where

import Universum

import Control.Lens
import Control.Monad.Writer.Strict

import Disciplina.Accounts
import Disciplina.Core (IndexedWithProof(..))

data WorldState tree hash identity
  = WorldState
    { _wsAccounts       :: tree hash identity (Account     hash)
    , _wsPublications   :: tree hash identity (Publication hash)
    , _wsSpecalizations :: tree hash identity (DAG         hash)
    , _wsStorage        :: tree hash identity  Storage
    }

type DAG         hash = ()
type Publication hash = hash
type Storage          = ()

data TreeName
    = TNAccounts
    | TNPulications
    | TNSpecializations
    | TNStorage

newWorldState
    :: IndexedWithProof (tree hash identity anything) m
    => MonadWriter proof m
    => m (WorldState tree hash identity)
newWorldState =
    WorldState
        <$> initialize
        <*> initialize
        <*> initialize

makeLenses ''WorldState
