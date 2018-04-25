
{-# LANGUAGE DeriveAnyClass #-}

module Disciplina.WorldState.Internal where

import qualified Prelude (show)

import Universum hiding (Hashable, get, trace, use)

import Codec.Serialise (Serialise)
import Control.Lens (makeLenses, makePrisms, to, use, zoom, (+~), (-~), (.=))
import Control.Monad.RWS (RWST (..), get, listen, tell)
import Data.Default (Default, def)

import Disciplina.Accounts
import Disciplina.Crypto.Hash (Hash)

import qualified Data.Tree.AVL as AVL
import qualified Debug.Trace as Debug

-- | Hash without a phantom type parameter. Temporary.
type Hash' = Hash ()

-- | Stub representation of entity.
data Entity = Entity Int
    deriving (Show, Eq, Ord, Bounded, Generic, Serialise, Default)

-- | Global state of the network, maintained by each server node.
data WorldState
  = WorldState
    { _accounts       :: AVLMap (Account Hash')
    , _publications   :: AVLMap Publication
    , _specalizations :: AVLMap DAG
    , _storage        :: AVLMap Storage
    , _code           :: AVLMap Code
    , _prevBlockHash  :: Hash'
    }

type AVLMap v = AVL.Map Hash' Entity v

-- | Proof of any action on global data.
data WorldStateProof
    = WorldStateProof
        { _accountsProof      :: Proof (Account Hash')
        , _publicationsProof  :: Proof Publication
        , _specalizatiosProof :: Proof DAG
        , _storageProof       :: Proof Storage
        , _codeProof          :: Proof Code
        , _prevBlockHashProof :: Hash'
        }
    deriving (Show, Generic, Serialise)

instance Eq WorldStateProof where
    WorldStateProof a b c d e x == WorldStateProof f g h i k y =
        and [a ==? f, b ==? g, c ==? h, d ==? i, e ==? k, x == y]
      where
        (==?) :: (Show s, Eq s, Serialise s) => Proof s -> Proof s -> Bool
        (==?) = (==) `on` getHash

        -- TODO: move into Data.Tree.AVL.Proof
        getHash proof = proof
            ^.   AVL._Proof    -- get the tree
             .to AVL.rootHash  -- reduce to hash

type Proof = AVL.Proof Hash' Entity

type DAG         = Hash'
type Publication = Hash'
type Storage     = Hash'
type Code        = Hash'

-- | The changes the transaction consists from.
data Change
    = TransferTokens Entity      Amount
    | Publicate      Publication
    | CreateAccount  Entity      Code
    deriving (Show, Generic, Serialise)

-- TODO: implement signing (and make 'Entity' to be actual public key).
data Transaction = Transaction
    { _tAuthor  :: Entity
    , _tChanges :: [Change]
    , _tNonce   :: Int
    }
    deriving (Generic, Serialise)

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
    { _wpBody      :: a
    , _wpProof     :: WorldStateProof
    , _wpEndHash   :: Hash'
    , _wpBeginHash :: Hash'
    }
    deriving (Show)

-- | World state on server.
data Server = Server
    { _sWorld :: WorldState
    }

-- | World state on client.
data Client = Client
    { _cProof :: WorldStateProof
    }

-- | Set of node identities to generate proof from. Proof prefab.
data DiffSets = DiffSets
    { _accountRevSet       :: Set Hash'
    , _publicationRevSet   :: Set Hash'
    , _specalizationRevSet :: Set Hash'
    , _storageRevSet       :: Set Hash'
    , _codeRevSet          :: Set Hash'
    }
    deriving (Default, Generic)

-- | Environment locat to transactions
data Environment = Environment
    { _eAuthor :: Entity
    }

-- | Variants of failure cases.
data TransactionError
    = AccountDoesNotExist       { doesNotExist   :: Sided Entity }
    | AccountExistButShouldNot  { shouldNotExist :: Sided Entity }

    | NotEnoughTokens           { holder       :: Entity
                                , wantsToSpend :: Amount
                                , hasOnly      :: Amount
                                }
    | NoncesMismatch            { entity        :: Entity
                                , hasNonce      :: Int
                                , requiredNonce :: Int
                                }
    | InitialHashesMismatch
    | FinalHashesMismatch
    | PreviousBlockHashMismatch
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

data Block trans = Block
    { _bTransactions  :: [trans]
    , _bPrevBlockHash :: Hash'
    }
    deriving (Show, Generic, Serialise)

type CanStore = AVL.KVStoreMonad Hash'

-- | Monad for operations.
type WorldT side m = RWST Environment DiffSets side m

makeLenses ''Environment
makeLenses ''Server
makeLenses ''Client
makeLenses ''DiffSets
makeLenses ''Transaction
makeLenses ''WithProof
makeLenses ''WorldState
makeLenses ''WorldStateProof

makePrisms ''Server
makePrisms ''Client

--instance HasHash a => HasHash (Block a) where
--    hash (Block list prev) = combineAll (prev : map hash list)

instance (AVL.KVStoreMonad h m, Monoid b) => AVL.KVStoreMonad h (RWST a b c m) where
    retrieve = lift . AVL.retrieve
    store k  = lift . AVL.store k

-- | Shows account balances for now.
instance Show WorldState where
    show world = world
      ^.accounts
       .to Prelude.show

-- | Get proof of current state.
class CanGetProof side where
    getProof :: CanStore m => WorldT side m WorldStateProof

instance CanGetProof Server where
    getProof = do
        world <- use sWorld
        diffWorldState def world

instance CanGetProof Client where
    getProof = use cProof

runWorldT :: CanStore m => Entity -> side -> WorldT side m a -> m (a, side)
runWorldT ent serverState action = do
    (a, out, _) <- runRWST action (Environment ent) serverState
    return (a, out)

evalWorldT :: CanStore m => Entity -> side -> WorldT side m a -> m a
evalWorldT ent serverState action = do
    (a, _, _) <- runRWST action (Environment ent) serverState
    return a

execWorldT :: CanStore m => Entity -> side -> WorldT side m a -> m side
execWorldT ent serverState action = do
    (_, side, _) <- runRWST action (Environment ent) serverState
    return side

emptyWorldState :: WorldState
emptyWorldState = WorldState AVL.empty AVL.empty AVL.empty AVL.empty AVL.empty def

-- | Generate a 'WorldState' for testing where given entites each have
--   the same amount of tokens.
giveEach :: CanStore m => [Entity] -> Amount -> WorldT side m WorldState
giveEach ents amount = do
    let emptyWorld  = emptyWorldState
    let initalState = def & aBalance .~ amount

    distribution <- AVL.fromList $ zip ents (repeat initalState)
    return $ emptyWorld & accounts .~ distribution

-- instance Default DiffSets where
--    def = DiffSets def def def def def

-- | Its sad, but 'Monoid' instance cannot be automatically derived properly.
instance Monoid DiffSets where
    mempty = def

    DiffSets a b c d e `mappend` DiffSets a1 b1 c1 d1 e1 =
        DiffSets (a <> a1) (b <> b1) (c <> c1) (d <> d1) (e <> e1)

-- | Smart constructors for change reports.
accountsChanged        :: Set Hash' -> DiffSets
publicationsChanged    :: Set Hash' -> DiffSets
specializationsChanged :: Set Hash' -> DiffSets
storagesChanged        :: Set Hash' -> DiffSets
codesChanged           :: Set Hash' -> DiffSets
accountsChanged        set' = def & accountRevSet       .~ set'
publicationsChanged    set' = def & publicationRevSet   .~ set'
specializationsChanged set' = def & specalizationRevSet .~ set'
storagesChanged        set' = def & storageRevSet       .~ set'
codesChanged           set' = def & codeRevSet          .~ set'

-- | Enrich an action with a proof.
withProof :: CanStore m => WorldT Server m a -> WorldT Server m (WithProof a)
withProof action = do
    was            <- use sWorld
    beginHash      <- getCurrentHash
    (res, revSets) <- listen action

    -- | TODO(kirill.andreev): check if 'listen' actually hides messages
    tell revSets

    diff    <- diffWorldState revSets was
    endHash <- getCurrentHash

    return (WithProof res diff endHash beginHash)

-- | Retrive a shard of world state from proof to perform operations on.
parsePartialWorldState :: WorldStateProof -> WorldState
parsePartialWorldState (WorldStateProof a b c d e h) =
    WorldState
        (a^.AVL._Proof)
        (b^.AVL._Proof)
        (c^.AVL._Proof)
        (d^.AVL._Proof)
        (e^.AVL._Proof)
        h

-- | Generate a world proof from sets of nodes touched during an action.
diffWorldState :: CanStore m => DiffSets -> WorldState -> WorldT side m WorldStateProof
diffWorldState changes world =
    let
      da = changes^.accountRevSet
      db = changes^.publicationRevSet
      dc = changes^.specalizationRevSet
      dd = changes^.storageRevSet
      de = changes^.codeRevSet

      WorldState a b c d e h = world

    in pure WorldStateProof
        <*> AVL.prune da a
        <*> AVL.prune db b
        <*> AVL.prune dc c
        <*> AVL.prune dd d
        <*> AVL.prune de e
        <*> pure h

-- | Check that entity signed as author exists
assertAuthorExists :: CanStore m => WorldT Server m ()
assertAuthorExists = do
    sender <- view eAuthor
    assertExistence (Sided sender Sender)

lookupAccount :: CanStore m => Sided Entity -> WorldT Server m (Maybe (Account Hash'))
lookupAccount entity = do
    -- TODO(kirill.andreev):
    --   Fix 'Data.Tree.AVL.Zipper.up' so that it doesn't rehash root
    --   every time, so we can run lookup in ReadonlyMode
    --   and remove this 'rollback'.
    (mAcc, trails) <- zoom (sWorld.accounts) $ do
        accs         <- get
        (res, _accs) <- AVL.lookup' (who entity) accs
        return res

    tell $ accountsChanged trails

    return mAcc

-- | Countercrutch, undoes state change from stateful action.
--   If `action` throws, state is not rolled back.
dryRun :: MonadState s m => m a -> m a
dryRun action = do
    was <- get
    res <- action
    put was
    return res

-- | Retrieves an account. Fail if account does not exist.
requireAccount :: CanStore m => Sided Entity -> WorldT Server m (Account Hash')
requireAccount entity = do
    mAcc <- lookupAccount entity
    case mAcc of
      Just it -> return it
      Nothing -> throwM $ AccountDoesNotExist entity

-- | Retrieves account of author.
getAuthorAccount :: CanStore m => WorldT Server m (Account Hash')
getAuthorAccount = do
    sender <- view eAuthor
    requireAccount (Sided sender Sender)

-- | Check if account is there.
existsAccount :: CanStore m => Sided Entity -> WorldT Server m Bool
existsAccount entity = maybe False (const True) <$> lookupAccount entity

-- | If account is not there, fail.
assertExistence :: CanStore m => Sided Entity -> WorldT Server m ()
assertExistence entity = do
    exists <- existsAccount entity
    when (not exists) $ do
        throwM $ AccountDoesNotExist entity

-- | If account IS there, fail.
assertAbsence :: CanStore m => Sided Entity -> WorldT Server m ()
assertAbsence entity = do
    exists <- existsAccount entity
    when (exists) $ do
        throwM $ AccountExistButShouldNot entity

-- | Create a new 'Account' with specified 'Code' to control it.
createAccount :: CanStore m => Entity -> Code -> WorldT Server m ()
createAccount whom code' = do
    assertAbsence (Sided whom Receiver)

    let
      account = def
        & aBalance .~ 0
        & aCode    .~ code'

    trails <- zoom (sWorld.accounts) $ do
        accs            <- get
        (trails, accs') <- AVL.insert' whom account accs

        put accs'
        return trails

    tell $ accountsChanged trails

-- | Upsert an (entity, account) into 'accounts' map.
unsafeSetAccount :: CanStore m => Entity -> Account Hash' -> WorldT Server m ()
unsafeSetAccount entity account = do
    trails <- zoom (sWorld.accounts) $ do
        accs            <- get
        (trails, accs') <- AVL.insert' entity account accs

        put accs'
        return trails

    tell $ accountsChanged trails

-- | Perform an action on the given entity's account.
--   Fail, if account is missing.
modifyAccount
    ::  CanStore m
    =>  Sided Entity
    -> (Account Hash' -> Account Hash')
    ->  WorldT Server m ()
modifyAccount entity f = do
    mAccount <- lookupAccount entity

    case mAccount of
      Just account -> do
        who entity `unsafeSetAccount` f account

      Nothing -> do
        throwM $ AccountDoesNotExist entity

-- | Implementation of 'Publicate' action.
--   TODO: Put a Merkle tree inside 'Publication', push inside it here.
publicate :: CanStore m => Publication -> WorldT Server m ()
publicate publication = do
    sender <- view eAuthor

    trails <- zoom (sWorld.publications) $ do
        accs            <- get
        (trails, accs') <- AVL.insert' sender publication accs

        put accs'
        return trails

    tell $ publicationsChanged trails

-- | Implementation of 'TransferTokens' action.
--   Gives another entity some tokens.
transferTokens :: CanStore m => Entity -> Amount -> WorldT Server m ()
transferTokens receiver amount = do
    who    <- view eAuthor
    sender <- getAuthorAccount

    when (sender^.aBalance < amount) $ do
        throwM $ NotEnoughTokens who amount (sender^.aBalance)

    modifyAccount (Sided who      Sender)   (aBalance -~ amount)
    modifyAccount (Sided receiver Receiver) (aBalance +~ amount)

-- | Perform an action using given identity.
impersonate :: CanStore m => Entity -> WorldT side m a -> WorldT side m a
impersonate whom action =
    local (eAuthor .~ whom) $ do
        action

-- | Perform the transaction, glue a proof to it.
playTransaction :: CanStore m => Transaction -> WorldT Server m (WithProof Transaction)
playTransaction transaction = do
    withProof $ do
        impersonate (transaction^.tAuthor) $ do
            author  <- view eAuthor
            account <- getAuthorAccount

            when (account^.aNonce /= transaction^.tNonce) $ do
                liftIO $ print account
                liftIO $ print transaction
                liftIO $ print ("----" :: String)
                throwM $ NoncesMismatch author
                    (account^.aNonce)
                    (transaction^.tNonce)

            modifyAccount (Sided author Sender) (aNonce +~ 1)

            for_ (transaction^.tChanges) $ \change ->
                case change of
                  TransferTokens to' amount      -> transferTokens to' amount
                  Publicate      publication     -> publicate publication
                  CreateAccount  newEntity code' -> createAccount newEntity code'

            return transaction

-- | Using proof, replay some actions as if you're the server node.
--   Does not generate proof for the actions replayed.
class CanUseProof side where
    usingProof :: CanStore m => WorldStateProof -> WorldT Server m a -> WorldT side m a

instance CanUseProof Client where
    usingProof proof action = do
        env <- ask
        let partialWorld = parsePartialWorldState proof
        (res, Server s, _w) <- lift $ runRWST action env (Server partialWorld)
        -- tell w
        diff <- diffWorldState def s
        put $ Client diff
        return res

instance CanUseProof Server where
    -- Since all "servers" do hold the same state, we can assume
    -- we can freely repeat an action on our current state here.
    usingProof _ action = action

-- | Unpack 'a' from proof, check that hashes before and after action match.
proving :: CanStore m => CanUseProof side => WithProof a -> (a -> WorldT Server m b) -> WorldT side m b
proving (WithProof body proof idealEndHash proposedBeginHash) action = do
    usingProof proof $ do
        beginHash <- getCurrentHash

        when (beginHash /= proposedBeginHash) $ do
            throwM InitialHashesMismatch

        res     <- action body
        endHash <- getCurrentHash

        when (endHash /= idealEndHash) $ do
            throwM FinalHashesMismatch

        return res

-- | Ability to retrieve hash of current state.
class CanGetHash side where
    getCurrentHash :: CanStore m => WorldT side m Hash'

instance CanGetHash Server where
    getCurrentHash = do
        Server st <- get
        proof <- diffWorldState def st
        return (hash proof)

instance CanGetHash Client where
    getCurrentHash = do
        Client proof <- get
        return (hash proof)

-- | Using proof, replay some actions as if you're the server node.
--   Does not generate proof for the actions replayed.
isolate :: CanStore m => Client -> WorldT Client m a -> WorldT Server m a
isolate client action = do
    env <- ask
    (res, Client _, _) <- lift $ runRWST action env client
    -- tell w
    return res

-- | Ability to apply transactions with proof to your state.
class CanReplayTransaction side where
    replayTransaction :: CanStore m => WithProof Transaction -> WorldT side m ()

instance CanReplayTransaction Client where
    replayTransaction transaction = do
        -- temporarily run server on a proof
        became <- usingProof (transaction^.wpProof) $ do
            replayTransaction transaction
            use _Server >>= diffWorldState def

        put (Client became)


instance CanReplayTransaction Server where
    replayTransaction transactionWithProof = do
        _ <- proving transactionWithProof playTransaction
        return ()

class CanSetPrevBlockHash side where
    setPrevBlockHash :: CanStore m => Hash' -> WorldT side m ()

instance CanSetPrevBlockHash Client where
    setPrevBlockHash hash' = do
        _Client.prevBlockHashProof .= hash'

instance CanSetPrevBlockHash Server where
    setPrevBlockHash hash' = do
        _Server.prevBlockHash .= hash'

-- | Freeze a bunch of changes in a transaction.
plan :: CanStore m => [Change] -> WorldT Server m Transaction
plan changes = do
    authorAcc <- getAuthorAccount
    let nonce = authorAcc^.aNonce
    author <- view eAuthor
    return $ Transaction author changes nonce

-- | Perform a bunch of transactions, add a collective proof to them.
playBlock :: CanStore m => [Transaction] -> WorldT Server m (WithProof [Transaction])
playBlock transactions =
    withProof $ do
        forM transactions $ \transaction -> do
            proven <- playTransaction transaction
            return (proven^.wpBody)

-- | Pack a bunch of transactions into a block.
generateBlock :: CanStore m => [Transaction] -> WorldT Server m (WithProof (Block Transaction))
generateBlock transactions = do
    withProof $ do
        prev <- use (_Server.prevBlockHash)
        -- TODO(kir): Generation of proof is a monadic actions now, so
        --            it is done and discarded. Make sure that we don't
        --            generate proof for each and every transaction here.
        _ <- playBlock transactions

        let block = Block transactions prev

        _Server.prevBlockHash .= hash block

        return block

-- | Take a block with proofs and apply it to current state.
replayBlock
    :: CanUseProof side
    => CanReplayTransaction side
    => CanStore m
    => WithProof (Block Transaction)
    -> WorldT side m ()
replayBlock blockWithProof = do
    proving blockWithProof $ \block -> do
        let Block transactions prev = block

        truePrev <- use (_Server.prevBlockHash)

        when (prev /= truePrev) $ do
            throwM PreviousBlockHashMismatch

        for_ transactions playTransaction
        setPrevBlockHash $ hash block

trace :: CanStore m => Show a => a -> WorldT side m ()
trace a = do
    () <- Debug.traceShow a $ return ()
    return ()
