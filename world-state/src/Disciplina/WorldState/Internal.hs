
{-# language DeriveAnyClass #-}

module Disciplina.WorldState.Internal where

import Universum

import Control.Lens (makeLenses)
import Control.Monad.Writer.Strict

import Data.Binary

import Disciplina.Accounts
import Disciplina.WorldState.BlakeHash

import qualified Data.Tree.AVL as AVL

data Entity = Entity Int
    deriving (Show, Eq, Ord, Bounded, Generic, Binary)

data WorldState
  = WorldState
    { _wsAccounts       :: AVLMap (Account Hash)
    , _wsPublications   :: AVLMap Publication
    , _wsSpecalizations :: AVLMap DAG
    , _wsStorage        :: AVLMap Storage
    }

type AVLMap v = AVL.Map Hash Entity v

data WorldStateProof
    = WorldStateProof
        { _accountsProof      :: Either Hash (Proof (Account Hash))
        , _publicationsProof  :: Either Hash (Proof Publication)
        , _specalizatiosProof :: Either Hash (Proof DAG)
        , _storageProof       :: Either Hash (Proof Storage)
        }

type Proof = AVL.Proof Hash Entity

type DAG         = Hash
type Publication = Hash
type Storage     = Hash

emptyWorldState :: WorldState
emptyWorldState =
    WorldState AVL.empty AVL.empty AVL.empty AVL.empty

makeLenses ''WorldState

data Change
    = TransferTokens Entity
    | Publicate      Publication
    | CreateEntity   Entity

data Transaction = Transaction
    { _tAuthor     :: Entity
    , _tChanges    :: [Change]
    , _tStateProof :: WorldStateProof
    , _tNonce      :: Int
    }

data Focused = Focused
    { _world   :: WorldState
    , _author  :: Entity
    , _changes :: (AVL.RevSet, AVL.RevSet, AVL.RevSet, AVL.RevSet)
    }

makeLenses ''Focused

diffWorldState :: State Focused WorldStateProof
diffWorldState = do
    (da, db, dc, dd)   <- use changes
    WorldState a b c d <- use world

    return $ WorldStateProof
        (if null da
         then Left  $ a^.AVL.rootHash
         else Right $ AVL.prune da a)
        (if null db
         then Left  $ b^.AVL.rootHash
         else Right $ AVL.prune db b)
        (if null dc
         then Left  $ c^.AVL.rootHash
         else Right $ AVL.prune dc c)
        (if null dd
         then Left  $ d^.AVL.rootHash
         else Right $ AVL.prune dd d)

lookupAccount :: Entity -> State Focused (Maybe (Account Hash))
lookupAccount ent = do
    () <- zoom
