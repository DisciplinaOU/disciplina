
{-# language DeriveAnyClass #-}

module Disciplina.WorldState.Internal where

import Universum

import Control.Lens (makeLenses, uses, (%=), (.=))
-- import Control.Monad.Writer.Strict

import Data.Binary (Binary)
import Data.Default

import Disciplina.Accounts
import Disciplina.WorldState.BlakeHash

import qualified Data.Tree.AVL as AVL

data Entity = Entity Int
    deriving (Show, Eq, Ord, Bounded, Generic, Binary)

data WorldState
  = WorldState
    { _accounts       :: AVLMap (Account Hash)
    , _publications   :: AVLMap Publication
    , _specalizations :: AVLMap DAG
    , _storage        :: AVLMap Storage
    , _code           :: AVLMap Code
    }

type AVLMap v = AVL.Map Hash Entity v

data WorldStateProof
    = WorldStateProof
        { _accountsProof      :: Either Hash (Proof (Account Hash))
        , _publicationsProof  :: Either Hash (Proof Publication)
        , _specalizatiosProof :: Either Hash (Proof DAG)
        , _storageProof       :: Either Hash (Proof Storage)
        , _codeProof          :: Either Hash (Proof Code)
        }

type Proof = AVL.Proof Hash Entity

type DAG         = Hash
type Publication = Hash
type Storage     = Hash
type Code        = Hash

emptyWorldState :: WorldState
emptyWorldState =
    WorldState AVL.empty AVL.empty AVL.empty AVL.empty AVL.empty

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
    , _changes :: (AVL.RevSet, AVL.RevSet, AVL.RevSet, AVL.RevSet, AVL.RevSet)
    }

makeLenses ''Focused

transaction :: State Focused a -> State Focused (a, WorldStateProof)
transaction action = do
    changes .= (def, def, def, def, def)
    res  <- action
    diff <- diffWorldState
    return (res, diff)

diffWorldState :: State Focused WorldStateProof
diffWorldState = do
    (da, db, dc, dd, de) <- use changes
    WorldState a b c d e <- use world

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
        (if null de
         then Left  $ e^.AVL.rootHash
         else Right $ AVL.prune de e)

lookupAccount :: Entity -> State Focused (Maybe (Account Hash))
lookupAccount ent = do
    -- TODO(kirill.andreev): Hide this mess into AVL package
    --                       Adapt the exported function to `MonadState Map m`
    (mAcc, _, accTrails) <- uses (world.accounts)
        (AVL.runZipped' (AVL.lookup' ent) AVL.UpdateMode)

    changes._1 %= (<> accTrails)

    return mAcc

existsAccount :: Entity -> State Focused Bool
existsAccount ent = maybe False (const True) <$> lookupAccount ent

-- TODO(kirill.andreev): Add MonadThrow/MonadCatch instead of returning Bool
createHumanAccount :: Entity -> Amount -> State Focused Bool
createHumanAccount entity balance = do
    existsAccount <- existsAccount entity

    if existsAccount
    then do
        return False

    else do
        -- TODO(kirill.andreev): Hide this block into 'zoom (world.accounts) $ do'
        accs <- use (world.accounts)
        let
          account =
            Account balance 0 def def

          ((), accs1, accTrails) =
            AVL.runZipped' (AVL.insert' entity account) AVL.UpdateMode accs

        world.accounts .= accs1
        changes._1 %= (<> accTrails)
        return True
