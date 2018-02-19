
{-# language DeriveAnyClass #-}

module Disciplina.WorldState.Internal where

import Universum

import Control.Lens (makeLenses, uses, (%=), (.=), zoom, (-~), (+~))
import Control.Monad.RWS (RWST(..), tell, listen)

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

data Change
    = TransferTokens Entity      Amount
    | Publicate      Publication
    | CreateAccount  Entity      Code

data Transaction = Transaction
    { _tAuthor     :: Entity
    , _tChanges    :: [Change]
    , _tNonce      :: Int
    }

data WithProof a = WithProof
    { _wpBody  :: a
    , _wpProof :: WorldStateProof
    }

data Focused = Focused
    { _fWorld   :: WorldState
    }

-- | Set of node names to generate proof from.
data RevisionSets = RevisionSets
    { _accountRevSet       :: AVL.RevSet
    , _publicationRevSet   :: AVL.RevSet
    , _specalizationRevSet :: AVL.RevSet
    , _storageRevSet       :: AVL.RevSet
    , _codeRevSet          :: AVL.RevSet
    }
    deriving (Default, Monoid, Generic)

data Environment = Environment
    { _eAuthor :: Entity
    }

data TransactionError
    = AccountDoesNotExist      (Sided Entity)
    | AccountExistButShouldNot (Sided Entity)
    | NotEnoughTokens           Entity Amount Amount
    | NoncesMismatch            Entity Int Int
    deriving (Show, Typeable)

-- | Enrich 'Entity' with its Side in the deal to improve future error messages.
data Sided a = Sided
    { who  :: a
    , side :: Side
    }
    deriving (Show, Typeable)

data Side = Sender | Receiver
    deriving (Show, Typeable)

deriving instance Exception TransactionError

type WorldM = RWST Environment RevisionSets Focused IO

makeLenses ''Environment
makeLenses ''Focused
makeLenses ''RevisionSets
makeLenses ''Transaction
makeLenses ''WithProof
makeLenses ''WorldState

emptyWorldState :: WorldState
emptyWorldState =
    WorldState AVL.empty AVL.empty AVL.empty AVL.empty AVL.empty

-- instance Default RevisionSets where
--    def = RevisionSets def def def def def

-- instance Monoid RevisionSets where
--     mempty = def

--     RevisionSets a b c d e `mappend` RevisionSets a1 b1 c1 d1 e1 =
--         RevisionSets (a <> a1) (b <> b1) (c <> c1) (d <> d1) (e <> e1)

accountsChanged        :: AVL.RevSet -> RevisionSets
publicationsChanged    :: AVL.RevSet -> RevisionSets
specializationsChanged :: AVL.RevSet -> RevisionSets
storagesChanged        :: AVL.RevSet -> RevisionSets
codesChanged           :: AVL.RevSet -> RevisionSets
accountsChanged        set = def & accountRevSet       .~ set
publicationsChanged    set = def & publicationRevSet   .~ set
specializationsChanged set = def & specalizationRevSet .~ set
storagesChanged        set = def & storageRevSet       .~ set
codesChanged           set = def & codeRevSet          .~ set

-- | Enrich an action with a proof.
withProof :: WorldM a -> WorldM (WithProof a)
withProof action = do
    was <- use fWorld
    (res, revSets) <- listen action
    tell revSets
    let diff = diffWorldState revSets was
    return (WithProof res diff)

-- | Check that entity signed as author exists
assertAuthorExists :: WorldM ()
assertAuthorExists = do
    sender <- view eAuthor
    assertExistence (Sided sender Sender)

-- | Generate a world proof from sets of nodes, touched during an action.
diffWorldState :: RevisionSets -> WorldState -> WorldStateProof
diffWorldState changes world =
    let
      da = changes^.accountRevSet
      db = changes^.publicationRevSet
      dc = changes^.specalizationRevSet
      dd = changes^.storageRevSet
      de = changes^.codeRevSet

      WorldState a b c d e = world

    in  WorldStateProof
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
    (mAcc, trails) <- rollback $ zoom (fWorld.accounts) $ do
        state $ AVL.lookup' (who entity)

    tell $ accountsChanged trails

    return mAcc

rollback :: MonadState s m => m a -> m a
rollback action = do
    was <- get
    res <- action
    put was
    return res

requireAccount :: Sided Entity -> WorldM (Account Hash)
requireAccount entity = do
    mAcc <- lookupAccount entity
    case mAcc of
      Just it -> return it
      Nothing -> throwM $ AccountDoesNotExist entity

getAuthorAccount :: WorldM (Account Hash)
getAuthorAccount = do
    sender <- view eAuthor
    requireAccount (Sided sender Sender)

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

createAccount :: Entity -> Code -> WorldM ()
createAccount whom code = do
    assertAbsence (Sided whom Receiver)

    let
      account = def
        & aBalance .~ 0
        & aCode    .~ code

    trails <- zoom (fWorld.accounts) $ do
        state $ AVL.insert' whom account

    tell $ accountsChanged trails

unsafeSetAccount :: Entity -> Account Hash -> WorldM ()
unsafeSetAccount entity account = do
    trails <- zoom (fWorld.accounts) $ do
        state $ AVL.insert' entity account

    tell $ accountsChanged trails

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
    sender <- view eAuthor

    trails <- zoom (fWorld.publications) $ do
        state $ sender `AVL.insert'` publication

    tell $ publicationsChanged trails

transferTokens :: Entity -> Amount -> WorldM ()
transferTokens receiver amount = do
    who    <- view eAuthor
    sender <- getAuthorAccount

    when (sender^.aBalance < amount) $ do
        throwM $ NotEnoughTokens who amount (sender^.aBalance)

    modifyAccount (Sided who      Sender)   (aBalance -~ amount)
    modifyAccount (Sided receiver Receiver) (aBalance +~ amount)

applyTransaction :: Transaction -> WorldM (WithProof Transaction)
applyTransaction transaction = do
    withProof $ do
        local (eAuthor .~ transaction^.tAuthor) $ do
            author  <- view eAuthor
            account <- getAuthorAccount

            when (account^.aNonce /= transaction^.tNonce) $ do
                throwM $ NoncesMismatch author
                    (account^.aNonce)
                    (transaction^.tNonce)

            modifyAccount (Sided author Sender) (aNonce +~ 1)

            for_ (transaction^.tChanges) $ \change ->
                case change of
                  TransferTokens to amount -> do
                    transferTokens to amount

                  Publicate publication -> do
                    publicate publication

                  CreateAccount newEntity code -> do
                    createAccount newEntity code

            return transaction

-- | Ignores proofs of concrete transactions, just return whole-block proof.
applyBlock :: [Transaction] -> WorldM (WithProof [Transaction])
applyBlock transactions =
    withProof $ do
        forM transactions $ \transaction -> do
            proven <- applyTransaction transaction
            return (proven^.wpBody)
