
{-# language DeriveAnyClass #-}

module Disciplina.WorldState.Internal where

import qualified Prelude (show)

import Universum hiding (Hashable, trace, use, get)

import Control.Lens (makeLenses, makePrisms, use, uses, to, (%=), (.=), zoom, (-~), (+~))
import Control.Monad.RWS (RWST(..), tell, listen, get)

import Data.Binary (Binary)
import Data.Default

import Disciplina.Accounts
import Disciplina.WorldState.BlakeHash (Hashable(..), Hash(..), combineAll)

import qualified Data.Tree.AVL as AVL
import qualified Debug.Trace   as Debug

-- | Stub representation of entity.
data Entity = Entity Int
    deriving (Show, Eq, Ord, Bounded, Generic, Binary, Default)

-- | Global state of the network, maintained by each server node.
data WorldState
  = WorldState
    { _accounts       :: AVLMap (Account Hash)
    , _publications   :: AVLMap Publication
    , _specalizations :: AVLMap DAG
    , _storage        :: AVLMap Storage
    , _code           :: AVLMap Code
    }

type AVLMap v = AVL.Map Hash Entity v

-- | Proof of any action on global data.
data WorldStateProof
    = WorldStateProof
        { _accountsProof      :: Proof (Account Hash)
        , _publicationsProof  :: Proof Publication
        , _specalizatiosProof :: Proof DAG
        , _storageProof       :: Proof Storage
        , _codeProof          :: Proof Code
        }
    deriving (Show, Generic, Binary)

instance Eq WorldStateProof where
    WorldStateProof a b c d e == WorldStateProof f g h i k =
        and [a ==? f, b ==? g, c ==? h, d ==? i, e ==? k]
      where
        (==?) :: (Show s, Eq s, Binary s) => Proof s -> Proof s -> Bool
        (==?) = (==) `on` getHash

        -- TODO: move into Data.Tree.AVL.Proof
        getHash proof = proof
            ^.AVL._Proof    -- get the tree
             .AVL.rootHash  -- reduce to hash

type Proof = AVL.Proof Hash Entity

type DAG         = Hash
type Publication = Hash
type Storage     = Hash
type Code        = Hash

-- | The changes the transaction consists from.
data Change
    = TransferTokens Entity      Amount
    | Publicate      Publication
    | CreateAccount  Entity      Code
    deriving (Show)

-- TODO: implement signing (and make 'Entity' to be actual public key).
data Transaction = Transaction
    { _tAuthor     :: Entity
    , _tChanges    :: [Change]
    , _tNonce      :: Int
    }

instance Show Transaction where
    show (Transaction who what nonce) =
        concat
            [ show who
            , ": "
            , show what
            , " ("
            , show nonce
            , ")"
            ]

-- | Wrapper that adds proof to either block or transaction.
data WithProof a = WithProof
    { _wpBody    :: a
    , _wpProof   :: WorldStateProof
    , _wpEndHash :: Hash
    }

-- | World state on server.
data Server = Server
    { _sWorld :: WorldState
    }

-- | World state on client.
data Client = Client
    { _cProof :: WorldStateProof
    }

-- | Set of node identities to generate proof from. Proof prefab.
data RevisionSets = RevisionSets
    { _accountRevSet       :: AVL.RevSet
    , _publicationRevSet   :: AVL.RevSet
    , _specalizationRevSet :: AVL.RevSet
    , _storageRevSet       :: AVL.RevSet
    , _codeRevSet          :: AVL.RevSet
    }
    deriving (Default, Generic)

data Environment = Environment
    { _eAuthor :: Entity
    }

-- | Variants of failure cases.
data TransactionError
    = AccountDoesNotExist       { doesNotExist   :: Sided Entity }
    | AccountExistButShouldNot  { shouldNotExist :: Sided Entity }

    | NotEnoughTokens           { holder         :: Entity
                                , wantsToSpend   :: Amount
                                , hasOnly        :: Amount
                                }
    | NoncesMismatch            { entity         :: Entity
                                , hasNonce       :: Int
                                , requiredNonce  :: Int
                                }
    | InitialHashesMismatch
    | FinalHashesMismatch
    deriving (Show, Typeable)

deriving instance Exception TransactionError

-- | Enrich 'Entity' with its 'Side' in the deal to improve error messages.
data Sided a = Sided
    { who  :: a
    , side :: Side
    }
    deriving (Show, Typeable)

data Side = Sender | Receiver
    deriving (Show, Typeable)

-- | Monad for operations.
type WorldM side = RWST Environment RevisionSets side IO

makeLenses ''Environment
makeLenses ''Server
makeLenses ''Client
makeLenses ''RevisionSets
makeLenses ''Transaction
makeLenses ''WithProof
makeLenses ''WorldState

makePrisms ''Server
makePrisms ''Client

-- | Shows account balances for now.
instance Show WorldState where
    show world = world
      ^.accounts
       .to AVL.toList
       .to (map (second (^.aBalance)))
       .to Prelude.show

-- | Get proof of current state.
class CanGetProof side where
    getProof :: WorldM side WorldStateProof

instance CanGetProof Server where
    getProof = uses sWorld (diffWorldState def)

instance CanGetProof Client where
    getProof = use cProof

runWorldM :: Entity -> side -> WorldM side a -> IO (a, side)
runWorldM ent serverState action = do
    (a, out, _) <- runRWST action (Environment ent) serverState
    return (a, out)

evalWorldM :: Entity -> side -> WorldM side a -> IO a
evalWorldM ent serverState action = do
    (a, _, _) <- runRWST action (Environment ent) serverState
    return a

execWorldM :: Entity -> side -> WorldM side a -> IO side
execWorldM ent serverState action = do
    (_, side, _) <- runRWST action (Environment ent) serverState
    return side

emptyWorldState :: WorldState
emptyWorldState =
    WorldState AVL.empty AVL.empty AVL.empty AVL.empty AVL.empty

-- | Generate a 'WorldState' for testing where given entites each have
--   the same amount of tokens.
giveEach :: [Entity] -> Amount -> WorldState
giveEach ents amount = emptyWorldState
    & accounts %~ give
  where
    give accs = foldr give' accs ents
      where
        give' ent = AVL.insertWithNoProof ent (def & aBalance .~ amount)

-- instance Default RevisionSets where
--    def = RevisionSets def def def def def

-- | Its sad, but 'Monoid' instance cannot be automatically derived properly.
instance Monoid RevisionSets where
    mempty = def

    RevisionSets a b c d e `mappend` RevisionSets a1 b1 c1 d1 e1 =
        RevisionSets (a <> a1) (b <> b1) (c <> c1) (d <> d1) (e <> e1)

-- | Smart constructors for change reports.
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
withProof :: WorldM Server a -> WorldM Server (WithProof a)
withProof action = do
    was <- use sWorld
    (res, revSets) <- listen action
    -- | TODO(kirill.andreev): check if 'listen' actually hides messages
    tell revSets
    let diff = diffWorldState revSets was
    endHash <- uses _Server (hash . diffWorldState def)
    return (WithProof res diff endHash)

-- | Retrive a shard of world state from proof to perform operations on.
parsePartialWorldState :: WorldStateProof -> WorldState
parsePartialWorldState (WorldStateProof a b c d e) =
    WorldState
        (a^.AVL._Proof)
        (b^.AVL._Proof)
        (c^.AVL._Proof)
        (d^.AVL._Proof)
        (e^.AVL._Proof)

-- | Generate a world proof from sets of nodes touched during an action.
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
        (AVL.prune da a)
        (AVL.prune db b)
        (AVL.prune dc c)
        (AVL.prune dd d)
        (AVL.prune de e)

-- | Check that entity signed as author exists
assertAuthorExists :: WorldM Server ()
assertAuthorExists = do
    sender <- view eAuthor
    assertExistence (Sided sender Sender)

lookupAccount :: Sided Entity -> WorldM Server (Maybe (Account Hash))
lookupAccount entity = do
    -- TODO(kirill.andreev):
    --   Fix 'Data.Tree.AVL.Zipper.up' so that it doesn't rehash root
    --   every time, so we can run lookup in ReadonlyMode
    --   and remove this 'rollback'.
    (mAcc, trails) <- rollback $ zoom (sWorld.accounts) $ do
        state $ AVL.lookup' (who entity)

    tell $ accountsChanged trails

    return mAcc

-- | Countercrutch, undoes state change from stateful action.
--   If `action` throws, state is not rolled back.
rollback :: MonadState s m => m a -> m a
rollback action = do
    was <- get
    res <- action
    put was
    return res

-- | Retrieves an account. Fail if account does not exist.
requireAccount :: Sided Entity -> WorldM Server (Account Hash)
requireAccount entity = do
    mAcc <- lookupAccount entity
    case mAcc of
      Just it -> return it
      Nothing -> throwM $ AccountDoesNotExist entity

-- | Retrieves account of author.
getAuthorAccount :: WorldM Server (Account Hash)
getAuthorAccount = do
    sender <- view eAuthor
    requireAccount (Sided sender Sender)

-- | Check if account is there.
existsAccount :: Sided Entity -> WorldM Server Bool
existsAccount entity = maybe False (const True) <$> lookupAccount entity

-- | If account is not there, fail.
assertExistence :: Sided Entity -> WorldM Server ()
assertExistence entity = do
    exists <- existsAccount entity
    when (not exists) $ do
        throwM $ AccountDoesNotExist entity

-- | If account IS there, fail.
assertAbsence :: Sided Entity -> WorldM Server ()
assertAbsence entity = do
    exists <- existsAccount entity
    when (exists) $ do
        throwM $ AccountExistButShouldNot entity

-- | Create a new 'Account' with specified 'Code' to control it.
createAccount :: Entity -> Code -> WorldM Server ()
createAccount whom code = do
    assertAbsence (Sided whom Receiver)

    let
      account = def
        & aBalance .~ 0
        & aCode    .~ code

    trails <- zoom (sWorld.accounts) $ do
        state $ AVL.insert' whom account

    tell $ accountsChanged trails

-- | Upsert an (entity, account) into 'accounts' map.
unsafeSetAccount :: Entity -> Account Hash -> WorldM Server ()
unsafeSetAccount entity account = do
    trails <- zoom (sWorld.accounts) $ do
        state $ AVL.insert' entity account

    tell $ accountsChanged trails

-- | Perform an action on the given entity's account.
--   Fail, if account is missing.
modifyAccount
    :: Sided Entity
    -> (Account Hash -> Account Hash)
    -> WorldM Server ()
modifyAccount entity f = do
    mAccount <- lookupAccount entity

    case mAccount of
      Just account -> do
        who entity `unsafeSetAccount` f account

      Nothing -> do
        throwM $ AccountDoesNotExist entity

-- | Implementation of 'Publicate' action.
--   TODO: Put a Merkle tree inside 'Publication', push inside it here.
publicate :: Publication -> WorldM Server ()
publicate publication = do
    sender <- view eAuthor

    trails <- zoom (sWorld.publications) $ do
        state $ sender `AVL.insert'` publication

    tell $ publicationsChanged trails

-- | Implementation of 'TransferTokens' action.
--   Gives another entity some tokens.
transferTokens :: Entity -> Amount -> WorldM Server ()
transferTokens receiver amount = do
    who    <- view eAuthor
    sender <- getAuthorAccount

    when (sender^.aBalance < amount) $ do
        throwM $ NotEnoughTokens who amount (sender^.aBalance)

    modifyAccount (Sided who      Sender)   (aBalance -~ amount)
    modifyAccount (Sided receiver Receiver) (aBalance +~ amount)

-- | Perform an action using given identity.
impersonate :: Entity -> WorldM side a -> WorldM side a
impersonate whom action =
    local (eAuthor .~ whom) $ do
        action

-- | Peroform the transaction, glue a proof to it.
connectTransaction :: Transaction -> WorldM Server (WithProof Transaction)
connectTransaction transaction = do
    withProof $ do
        impersonate (transaction^.tAuthor) $ do
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

-- | Perform a bunch of transactions, add a collective proof to them.
connectBlock :: [Transaction] -> WorldM Server (WithProof [Transaction])
connectBlock transactions =
    withProof $ do
        forM transactions $ \transaction -> do
            proven <- connectTransaction transaction
            return (proven^.wpBody)

-- | Using proof, replay some actions as if you're the server node.
--   Does not generate proof for the actions replayed.
usingProof :: WorldStateProof -> WorldM Server a -> WorldM Client a
usingProof proof action = do
    env <- ask
    let state = parsePartialWorldState proof
    (res, Server s, w) <- lift $ runRWST action env (Server state)
    -- tell w
    put $ Client $ diffWorldState def s
    return res

-- | Ability to apply transactions with proof to your state.
class CanAssumeTransaction side where
    assumeTransaction :: WithProof Transaction -> WorldM side ()

instance CanAssumeTransaction Client where
    assumeTransaction transaction = do
        -- temporarily run server on a proof
        became <- usingProof (transaction^.wpProof) $ do
            assumeTransaction transaction
            uses _Server (diffWorldState def)

        put (Client became)


instance CanAssumeTransaction Server where
    assumeTransaction transaction = do
        here <- getProof

        let proof = parsePartialWorldState (transaction^.wpProof)
        let there = diffWorldState def proof

        when (hash here /= hash there) $ do
            throwM InitialHashesMismatch

        _        <- connectTransaction (transaction^.wpBody)
        endProof <- uses _Server (diffWorldState def)

        let endHash = hash endProof

        when (endHash /= transaction^.wpEndHash) $ do
            throwM FinalHashesMismatch

-- | Freeze a bunch of changes in a transaction.
plan :: [Change] -> WorldM Server Transaction
plan changes = do
    authorAcc <- getAuthorAccount
    let nonce = authorAcc^.aNonce
    author <- view eAuthor
    return $ Transaction author changes nonce

trace :: Show a => a -> WorldM side ()
trace a = do
    () <- Debug.traceShow a $ return ()
    return ()
