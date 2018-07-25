{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- This module is a complete BARDAQ, please rewrite it.  And after you
-- do, add docs and sections. It's harder to read/understand than you
-- think!

-- | AVL plus integration into snowdrop.

module Dscp.Snowdrop.Storage.Avlp
    (
      ClientMode(..)
    , RememberForProof (..)

    , AVLCacheT (..)
    , avlServerDbActions
    , avlClientDbActions
    , AVLChgAccum
    , AVLServerState
    , AvlHash(..)
    , RootHash(..)
    , initAVLPureStorage
    , ClientError (..)
    , deserialiseM
    ) where

import Codec.Serialise (Serialise (..))
import Control.Monad.Free (Free (Free))
import Data.Default (Default (def))
import qualified Data.Map.Strict as M
import qualified Data.Text.Buildable as Buildable
import Data.Tree.AVL (KVStoreMonad (..), MapLayer (..))
import qualified Data.Tree.AVL as AVL
import Snowdrop.Model.Execution (DbAccessActions (..), DbActionsException (DbProtocolError),
                                 DbModifyActions (..))
import Snowdrop.Model.State.Core (StateP, StateR)
import Snowdrop.Util (CSMappendException (..), ChangeSet, HasGetter (..), IdSumPrefixed (..),
                      Prefix, ValueOp (..), changeSetToList)

import Dscp.Witness.AVL (AvlHash (..))


-- How do I use it?

data ClientMode proof
  = ProofMode { cmProof :: proof }
  | RemoteMode

-- | Toggle on whether to record proof within DbModifyActions
data RememberForProof = RememberForProof Bool

-- | Data type for tracking keys which were requested
-- from access actions
data AMSRequested k
    = AMSWholeTree
    -- ^ Constructor, identifying that all keys of tree were requested
    -- (which happens with current implementation of iteration)
    | AMSKeys (Set k)
    -- ^ Constructor, containing set of keys that were requested

instance Ord k => Semigroup (AMSRequested k) where
    AMSWholeTree <> _ = AMSWholeTree
    _ <> AMSWholeTree = AMSWholeTree
    AMSKeys s1 <> AMSKeys s2 = AMSKeys $ s1 <> s2

instance Ord k => Monoid (AMSRequested k) where
    mempty = AMSKeys mempty
    mappend = (<>)

-- | Data type used as state of `avlStateDbActions`
data AVLServerState k = AMS
    { amsRootHash  :: RootHash
      -- ^ Root hash of tree kept in storage
    , amsState     :: AVLPureStorage
      -- ^ Storage of whole AVL tree (including old nodes)
    , amsRequested :: AMSRequested k
      -- ^ Set of keys that were requested since the last `apply` operation.
      -- Note, that keys which were requested with `RememberForProof False` passed to
      -- `avlServerDbActions` are not being added to this set.
    }

newtype RootHash = RootHash { unRootHash :: AvlHash }
    deriving Eq

instance HasGetter (AVLServerState k) RootHash where
    gett = amsRootHash

instance HasGetter (AVLServerState k) AVLPureStorage where
    gett = amsState

-- | Pure implementation of permanent storage
newtype AVLPureStorage = AVLPureStorage { unAVLPureStorage :: Map AvlHash ByteString }

-- | Accumulator for changes emerging from `save` operations
-- being performed on AVL tree
newtype AVLCache = AVLCache { unAVLCache :: Map AvlHash ByteString }
    deriving (Default, Semigroup, Monoid)

-- | Monad transformer for caching `save` operations resulting from AVL+ actions
newtype AVLCacheT m a = AVLCacheT (StateT AVLCache m a)
    deriving (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadState AVLCache, MonadTrans)

runAVLCacheT :: MonadThrow m => AVLCacheT (ReaderT ctx m) a -> AVLCache -> ctx -> m (a, AVLCache)
runAVLCacheT (AVLCacheT ma) initSt ctx = runReaderT (runStateT ma initSt) ctx

instance (Show h, Show k, Show v) => Buildable (AVL.Map h k v) where
    build = Buildable.build . AVL.showMap

instance (Show h, Show k, Show v) => Buildable (AVL.Proof h k v) where
    build (AVL.Proof tree) = Buildable.build tree

deserialiseM :: (MonadThrow m, AVL.Serialisable v) => ByteString -> m v
deserialiseM =
    either (throwM . DbProtocolError . ("Deserialisation error "<>) . toText) pure .
    AVL.deserialise

type RetrieveF m = AvlHash -> m (Maybe ByteString)

-- TODO replace with AVL.KVRetrieveM after this type is introduced into library
class Monad m => RetrieveImpl m where
    retrieveImpl :: RetrieveF m

instance RetrieveImpl (ReaderT (RetrieveF IO) IO) where
    retrieveImpl k = ask >>= lift . ($ k)

instance MonadThrow m => RetrieveImpl (ReaderT (AVLServerState k) m) where
    retrieveImpl k = asks (M.lookup k . unAVLPureStorage . gett)

instance MonadThrow m => RetrieveImpl (ReaderT AVLPureStorage m) where
    retrieveImpl k = asks (M.lookup k . unAVLPureStorage)

instance (MonadThrow m, RetrieveImpl m) => KVStoreMonad AvlHash (AVLCacheT m) where
    retrieve k = checkInAccum >>= deserialiseM
      where
        checkInAccum = M.lookup k . unAVLCache <$> get >>= maybe checkInState pure
        checkInState = lift (retrieveImpl k) >>= maybe (throwM $ AVL.NotFound k) pure
    store k v = modify' $ AVLCache . M.insert k (AVL.serialise v) . unAVLCache

-- | Change accumulator type for AVL tree.
data AVLChgAccum' k v = AVLChgAccum
    { acaMap     :: AVL.Map AvlHash k v
    -- ^ AVL map, which contains avl tree with most-recent updates
    , acaStorage :: AVLCache
    -- ^ AVL tree cache, which stores results of all `save` operations performed on AVL tree
    , acaTouched :: Set AvlHash
    -- ^ Set of nodes, which were touched during all of change operations applied on tree
    }

-- | Change accumulator type for AVL tree, wrapped with Maybe.
-- `Nothing` is treated identically to `Just $ AVLChgAccum (Pure rootHash) def mempty`,
-- where `rootHash` is root hash of current state
type AVLChgAccum k v = Maybe (AVLChgAccum' k v)

saveAVL :: (AVL.Stores AvlHash k v m, MonadCatch m) => AVL.Map AvlHash k v -> m RootHash
saveAVL avl = AVL.save avl $> (RootHash . AVL.rootHash) avl

resolveAvlCA :: HasGetter state RootHash => state -> AVLChgAccum k v -> AVLChgAccum' k v
resolveAvlCA _ (Just cA) = cA
resolveAvlCA st _ =
    AVLChgAccum
        { acaMap = pure $ unRootHash $ gett st
        , acaStorage = mempty
        , acaTouched = mempty
        }

data ClientError = BrokenProofError | UnexpectedRootHash
    deriving Show

instance Exception ClientError

type KVConstraint k v
     = ( IdSumPrefixed k
       , Typeable k
       , Ord k
       , Show k
       , Serialise k
       , Serialise v
       )

mkAVL :: RootHash -> AVL.Map AvlHash k v
mkAVL = pure . unRootHash

initAVLPureStorage ::
       forall k v m.
       ( MonadIO m
       , MonadCatch m
       , KVConstraint k v
       , Show k, Show v -- this is for putStrLn only, i guess for debug
       )
    => Map k v
    -> m (AVLServerState k)
initAVLPureStorage (M.toList -> kvs) = reThrowAVLEx @k $ do
    (rootH, AVLPureStorage . unAVLCache -> st) <-
      runAVLCacheT
        (foldM (\b (x, y) -> snd <$> AVL.insert @AvlHash @k @v x y b) AVL.empty kvs >>= saveAVL)
        def
        (AVLPureStorage mempty)
    fullAVL <- runAVLCacheT (materialize @AvlHash @k @v $ mkAVL rootH) def st
    putStrLn $ "Built AVL+ tree:\n" <> (AVL.showMap @AvlHash @k @v $ fst fullAVL)
    pure $ AMS { amsRootHash = rootH, amsState = st, amsRequested = mempty }

instance MonadThrow n => RetrieveImpl (ReaderT (ClientTempState k v n) n) where
    retrieveImpl k = do
        (cts :: Either AVLPureStorage (RetrieveF n)) <-
            asks ctRetrieve
        (res :: Maybe ByteString) <-
            either
                (runReaderT $ retrieveImpl k)
                (\retrieveF -> lift $ retrieveF k)
                cts
        pure res

data ClientTempState k v n = ClientTempState
    { ctRetrieve :: Either AVLPureStorage (RetrieveF n)
    , ctRootHash :: RootHash
    }

clientModeToTempSt ::
       (KVConstraint k v, MonadCatch m, MonadIO n)
    => RetrieveF n
    -> ClientMode (AVL.Proof AvlHash k v)
    -> RootHash
    -> m (ClientTempState k v n)
clientModeToTempSt _ (ProofMode p@(AVL.Proof avl)) rootH =
    flip ClientTempState rootH <$> fmap Left convert
  where
    convert = do
        when (not $ AVL.checkProof @AvlHash (unRootHash rootH) p) $
            throwM BrokenProofError
        AVLPureStorage . unAVLCache . snd <$>
            runAVLCacheT (saveAVL avl) def (AVLPureStorage mempty)
clientModeToTempSt retrieveF RemoteMode rootH = pure $ ClientTempState (Right retrieveF) rootH


instance HasGetter (ClientTempState k v n) RootHash where
    gett = ctRootHash


avlClientDbActions ::
       forall k v m n. (KVConstraint k v, MonadIO m, MonadIO n, MonadCatch n)
    => RetrieveF n
    -> RootHash
    -> m (ClientMode (AVL.Proof AvlHash k v) -> DbModifyActions (AVLChgAccum k v) k v n ())
avlClientDbActions retrieveF = fmap mkActions . newTVarIO
  where
    mkActions var ctMode =
        DbModifyActions (mkAccessActions var ctMode) (reThrowAVLEx @k . apply var)
    mkAccessActions var ctMode =
        DbAccessActions
          -- adding keys to amsRequested
          (\cA req -> reThrowAVLEx @k $ query cA req =<< createState)
          (\cA cs -> reThrowAVLEx @k $ modAccum cA cs =<< createState)
          -- setting amsRequested to AMSWholeTree as iteration with
          -- current implementation requires whole tree traversal
          (\cA p b f -> reThrowAVLEx @k $ iter cA p b f =<< createState)
      where
        createState = clientModeToTempSt retrieveF ctMode =<< atomically (readTVar var)
    apply :: TVar RootHash -> AVLChgAccum k v -> n ()
    apply var (Just (AVLChgAccum accAvl _acc _accTouched)) =
        atomically $ writeTVar var (RootHash $ AVL.rootHash accAvl)
    apply _ Nothing = pure ()


avlServerDbActions ::
       forall k v m n. (KVConstraint k v, MonadIO m, MonadIO n, MonadCatch n)
    => AVLServerState k
    -> m ( RememberForProof -> DbModifyActions (AVLChgAccum k v) k v n (AVL.Proof AvlHash k v)
          -- `DbModifyActions` provided by `RememberForProof` object
          -- (`RememberForProof False` for disabling recording for queries performed)
          , RetrieveF n
          -- Function to retrieve data from server state internal AVL storage
          )
avlServerDbActions = fmap mkActions . newTVarIO
  where
    retrieveHash var h = atomically $ M.lookup h . unAVLPureStorage . amsState <$> readTVar var
    mkActions (var :: TVar (AVLServerState k)) =
        (\recForProof -> DbModifyActions (mkAccessActions var recForProof)
                                         (reThrowAVLEx @k . apply var)
        , retrieveHash var)
    mkAccessActions var recForProof =
        DbAccessActions
          -- adding keys to amsRequested
          (\cA req -> reThrowAVLEx @k $ query cA req =<<
                      atomically (retrieveAMS var recForProof $ AMSKeys req ))
          (\cA cs -> reThrowAVLEx @k $ modAccum cA cs =<<
                     atomically (readTVar var))
          -- setting amsRequested to AMSWholeTree as iteration with
          -- current implementation requires whole tree traversal
          (\cA p b f -> reThrowAVLEx @k $ iter cA p b f =<<
                        atomically (retrieveAMS var recForProof AMSWholeTree ))

    retrieveAMS var (RememberForProof True) amsReq = do
        ams <- readTVar var
        writeTVar var (ams { amsRequested = amsRequested ams <> amsReq }) $> ams
    retrieveAMS var _ _ = readTVar var

    apply :: TVar (AVLServerState k) -> AVLChgAccum k v -> n (AVL.Proof AvlHash k v)
    apply var (Just (AVLChgAccum accAvl acc accTouched)) =
        applyDo >>= \oldAms ->
          fst <$> runAVLCacheT (computeProof (amsRootHash oldAms) (amsRequested oldAms))
                               def
                               (amsState oldAms)
      where
        applyDo :: n (AVLServerState k)
        applyDo = atomically $ do
          ams <- readTVar var
          (h', acc') <- runAVLCacheT (saveAVL accAvl) acc (amsState ams)
          let newState =
                AMS { amsRootHash = h'
                    , amsState = AVLPureStorage $ unAVLCache acc' <> unAVLPureStorage (amsState ams)
                    , amsRequested = mempty
                    }
          writeTVar var newState $> ams

        computeProof :: RootHash
                     -> AMSRequested k
                     -> AVLCacheT (ReaderT AVLPureStorage n) (AVL.Proof AvlHash k v)
        computeProof (mkAVL -> oldAvl) requested =
            case requested of
              AMSWholeTree -> computeProofWhole
              AMSKeys ks   -> computeProofKeys ks
          where
            computeProofWhole :: AVLCacheT (ReaderT AVLPureStorage n) (AVL.Proof AvlHash k v)
            computeProofWhole = AVL.Proof <$> materialize oldAvl

            computeProofKeys :: Set k
                             -> AVLCacheT (ReaderT AVLPureStorage n) (AVL.Proof AvlHash k v)
            computeProofKeys ks = do
              (avl', allTouched) <- foldM computeTouched (oldAvl, mempty) ks
              AVL.prune (allTouched <> accTouched) =<< materialize avl'

            computeTouched
              :: (AVL.Map AvlHash k v, Set AvlHash)
              -> k
              -> AVLCacheT (ReaderT AVLPureStorage n) (AVL.Map AvlHash k v, Set AvlHash)
            computeTouched (avl, touched) key = do
              ((_res, touched'), avl') <- AVL.lookup' key avl
              pure (avl', touched' <> touched)

    apply var Nothing = AVL.Proof . mkAVL . amsRootHash <$> atomically (readTVar var)


materialize :: forall h k v m . AVL.Stores h k v m => AVL.Map h k v -> m (AVL.Map h k v)
materialize initAVL = flip AVL.openAndM initAVL $ \case
    MLBranch h m c t l r -> fmap Free $ MLBranch h m c t <$> materialize l <*> materialize r
    rest -> pure $ Free rest

reThrowAVLEx :: forall k m a . (MonadCatch m, Show k, Typeable k) => m a -> m a
reThrowAVLEx m =
    m `catch` (\(e :: ClientError) -> throwM $ DbProtocolError $ show e)
      `catch` (\(e :: AVL.DeserialisationError) -> throwM $ DbProtocolError $ show e)
      `catch` (\(e :: AVL.NotFound k) -> throwM $ DbProtocolError $ show e)

modAccum' ::
       forall k v ctx m.
       (KVConstraint k v, RetrieveImpl (ReaderT ctx m), MonadIO m, MonadCatch m)
    => AVLChgAccum' k v
    -> ChangeSet k v
    -> ctx
    -> m (Either (CSMappendException k) (AVLChgAccum k v))
modAccum' (AVLChgAccum initAvl initAcc initTouched) (changeSetToList -> ops) pState =
    (Right . Just . doUncurry AVLChgAccum <$>
     runAVLCacheT (foldM modAVL (initAvl, initTouched) ops) initAcc pState) `catch` \e@(CSMappendException _) ->
        pure $ Left e
  where
    doUncurry :: (a -> b -> c -> d) -> ((a, c), b) -> d
    doUncurry f ((a, c), b) = f a b c
    modAVL ::
           (AVL.Map AvlHash k v, Set AvlHash)
        -> (k, ValueOp v)
        -> AVLCacheT (ReaderT ctx m) (AVL.Map AvlHash k v, Set AvlHash)
    modAVL (avl, touched) (k, valueop) = processResp =<< AVL.lookup' k avl
      where
        processResp ((lookupRes, (<> touched) -> touched'), avl') =
            case (valueop, lookupRes) of
                (NotExisted, _) -> pure (avl', touched')
                (New v, _)      -> (, touched') . snd <$> AVL.insert' k v avl'
                (Rem, Just _)   -> (, touched') . snd <$> AVL.delete' k avl'
                (Upd v, Just _) -> (, touched') . snd <$> AVL.insert' k v avl'
                _               -> throwM $ CSMappendException k

modAccum ::
       forall k v ctx m.
       (KVConstraint k v, HasGetter ctx RootHash, RetrieveImpl (ReaderT ctx m), MonadIO m, MonadCatch m)
    => AVLChgAccum k v
    -> ChangeSet k v
    -> ctx
    -> m (Either (CSMappendException k) (AVLChgAccum k v))
-- empty changeset won't alter accumulator
modAccum accM (changeSetToList -> []) _ = pure $ Right accM
modAccum (Just acc) cs sth              = modAccum' @k @v @ctx @m acc cs sth
modAccum cA cs sth                      = modAccum' @k @v @ctx @m (resolveAvlCA sth cA) cs sth

query ::
       forall k v ctx m.
       ( KVConstraint k v
       , HasGetter ctx RootHash
       , RetrieveImpl (ReaderT ctx m)
       , MonadIO m
       , MonadCatch m
       )
    => AVLChgAccum k v
    -> StateR k
    -> ctx
    -> m (StateP k v)
query (Just (AVLChgAccum initAvl initAcc _)) req sth = fmap fst $ runAVLCacheT queryDo initAcc sth
  where
    queryDo = fst <$> foldM queryDoOne (mempty, initAvl) req
    queryDoOne (m, avl) key = first processResp <$> AVL.lookup' key avl
      where
        processResp (Just v, _touched) = M.insert key v m
        processResp _                  = m
query cA req sth = query (Just $ resolveAvlCA sth cA) req sth


iter ::
       forall k v ctx b m.
       ( KVConstraint k v
       , HasGetter ctx RootHash
       , RetrieveImpl (ReaderT ctx m)
       , MonadIO m
       , MonadCatch m
       )
    => AVLChgAccum k v
    -> Prefix
    -> b
    -> ((k, v) -> b -> b)
    -> ctx
    -> m b
iter (Just (AVLChgAccum initAvl initAcc _)) pr initB f sth =
    fmap fst $ runAVLCacheT (AVL.fold (initB, f', id) initAvl) initAcc sth
  where
    f' kv@(k, _) b =
      if idSumPrefix k == pr
         then f kv b
         else b
iter cA pr b f sth = iter (Just $ resolveAvlCA sth cA) pr b f sth
