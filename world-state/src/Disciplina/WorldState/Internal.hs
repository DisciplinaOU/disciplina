
{-# language DeriveAnyClass #-}

module Disciplina.WorldState.Internal where

import Universum

import Control.Lens (makeLenses, uses, (%=), (.=), zoom, (-~), (+~))
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
    , _tNonce      :: Int
    }

data WithProof a = WithProof
    { _wpBody  :: a
    , _wpProof :: WorldStateProof
    }

makeLenses ''WithProof

data Focused = Focused
    { _world   :: WorldState
    , _author  :: Entity
    , _changes :: (AVL.RevSet, AVL.RevSet, AVL.RevSet, AVL.RevSet, AVL.RevSet)
    }

makeLenses ''Focused

data TransactionError
    = AccountDoesNotExist      (Sided Entity)
    | AccountExistButShouldNot (Sided Entity)
    | NotEnoughTokens           Entity Amount Amount
    deriving (Show, Typeable)

data Sided a = Sided { who :: a, side :: Side }
    deriving (Show, Typeable)

data Side = Sender | Receiver
    deriving (Show, Typeable)

deriving instance Exception TransactionError

type WorldM = StateT Focused IO

withProof :: WorldM a -> WorldM (WithProof a)
withProof action = do
    assertAuthorExists
    changes .= (def, def, def, def, def)
    res  <- action
    diff <- diffWorldState
    return (WithProof res diff)

assertAuthorExists :: WorldM ()
assertAuthorExists = do
    sender <- use author
    assertExistence (Sided sender Sender)

diffWorldState :: WorldM WorldStateProof
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

lookupAccount :: Sided Entity -> WorldM (Maybe (Account Hash))
lookupAccount entity = do
    -- TODO(kirill.andreev):
    --   Fix 'Data.Tree.AVL.Zipper.up' so that it doesn't rehash root
    --   every time, so we can run lookup in ReadonlyMode
    --   and remove this 'rollback'.
    (mAcc, trails) <- rollback $ zoom (world.accounts) $ do
        state $ AVL.lookup' (who entity)

    changes._1 %= (<> trails)

    return mAcc

requireAccount :: Sided Entity -> WorldM (Account Hash)
requireAccount entity = do
    mAcc <- lookupAccount entity
    case mAcc of
      Just it -> return it
      Nothing -> throwM $ AccountDoesNotExist entity

getAuthor :: WorldM (Account Hash)
getAuthor = do
    sender <- use author
    requireAccount (Sided sender Sender)

rollback :: MonadState s m => m a -> m a
rollback action = do
    was <- get
    res <- action
    put was
    return res

existsAccount :: Sided Entity -> WorldM Bool
existsAccount entity = maybe False (const True) <$> lookupAccount entity

assertExistence :: Sided Entity -> WorldM ()
assertExistence entity = do
    exists <- existsAccount entity
    when (not exists) $ do
        throwM $ AccountDoesNotExist entity

assertAbsence :: Sided Entity -> WorldM ()
assertAbsence entity = do
    exists <- existsAccount entity
    when (exists) $ do
        throwM $ AccountExistButShouldNot entity

-- TODO(kirill.andreev): Add MonadThrow/MonadCatch instead of returning Bool
createAccount :: Entity -> Code -> WorldM ()
createAccount whom code = do
    assertAbsence (Sided whom Receiver)

    let
      account = def
        & aBalance .~ 0
        & aCode    .~ code

    trails <- zoom (world.accounts) $ do
        state $ AVL.insert' whom account

    changes._1 %= (<> trails)

unsafeSetAccount :: Entity -> Account Hash -> WorldM ()
unsafeSetAccount entity account = do
    trails <- zoom (world.accounts) $ do
        state $ AVL.insert' entity account

    changes._1 %= (<> trails)

modifyAccount :: Sided Entity -> (Account Hash -> Account Hash) -> WorldM ()
modifyAccount entity f = do
    mAccount <- lookupAccount entity

    case mAccount of
      Just account -> do
        who entity `unsafeSetAccount` f account

      Nothing -> do
        throwM $ AccountDoesNotExist entity

publicate :: Publication -> WorldM ()
publicate publication = do
    sender <- use author

    trails <- zoom (world.publications) $ do
        state $ sender `AVL.insert'` publication

    changes._2 %= (<> trails)

transferTokens :: Entity -> Amount -> WorldM ()
transferTokens receiver amount = do
    who    <- use author
    sender <- getAuthor

    when (sender^.aBalance < amount) $ do
        throwM $ NotEnoughTokens who amount (sender^.aBalance)

    modifyAccount (Sided who      Sender)   (aBalance -~ amount)
    modifyAccount (Sided receiver Receiver) (aBalance +~ amount)
