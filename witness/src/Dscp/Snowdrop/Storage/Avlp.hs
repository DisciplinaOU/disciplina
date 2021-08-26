
-- | This module represents AVL+ backend for SD.
--   It defines universal 'DbModifyActions', which you can use with
--   any AVL+-compatible storage as 'DB.Plugin'.
--   RocksDB and in-memory one are provided.
--
--   The 'rocksDBPlugin' constructs db plugin from 'RocksDB' value.
--
--   The 'pureDBPlugin' constructs db plugin from 'TVar (Pure.State h k v)'.
--
module Dscp.Snowdrop.Storage.Avlp
    (
      -- | Change accumulator for AVL+ tree.
      AVLChgAccum
    , mkAvlDbModifyActions
    , initAVLStorage
    , AVLMutates
    ) where

import Codec.Serialise
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Typeable (typeOf)
import UnliftIO (MonadUnliftIO)

import qualified Data.Tree.AVL as AVL

import Snowdrop.Core (CSMappendException (..), ChangeSet (..), ChgAccumModifier (..),
                      IdSumPrefixed (..), Prefix, StateP, StateR, Undo (..), ValueOp (..),
                      changeSetToList, idSumPrefix)
import Snowdrop.Execution (DbAccessActions (..), DbModifyActions (..))

import qualified Dscp.DB.CanProvideDB as DB
import Dscp.Util

-- | Following constraints contain 't', because we only use
--   AVL actions in 'm' contexts.
type AVLMutates h k v m =
    ( AVLBase      h k v m
    -- , AVL.Mutates  h k v m
    , CanSerialise h k v
    , MonadIO    m
    , MonadCatch m
    )

-- | Basic requirements to implement RocksDB storage.
type AVLBase h k v m =
    ( AVL.Base     h k v m
    , CanSerialise h k v
    , MonadIO       m
    , MonadUnliftIO m
    )

-- | So we can put (hash -> node) relation into RocksDB.
type CanSerialise h k v =
    ( Serialise  h
    , Serialise (AVL.MapLayer h k v h)
    )

-- | Instance for reading from the database.
instance AVLBase h k v m => AVL.KVRetrieve h (AVL.MapLayer h k v h) (ReaderT DB.Plugin m) where
    retrieve h =
        DB.get h >>= nothingToThrow (AVL.NotFound h)

-- | Instance for reading from the database.
instance AVLBase h k v m => AVL.KVStore h (AVL.MapLayer h k v h) (ReaderT DB.Plugin m) where
    massStore = DB.batch . map (uncurry DB.Put)

-- | Instance for getting/setting a current root in the database.
-- | Also, for overwriting, if needed.
instance AVLBase h k v m => AVL.KVMutate h (AVL.MapLayer h k v h) (ReaderT DB.Plugin m)
  where
    getRoot   = DB.get  (rootName @h @k @v) >>= nothingToThrow AVL.NoRootExists
    setRoot r = DB.batch [DB.Put (rootName @h @k @v) r]
    erase   h = DB.batch @_ @() [DB.Delete h]

-- | A tip name for AVL tree with given argument types.
rootName :: forall h k v . (Typeable h, Typeable k, Typeable v) => ByteString
rootName
    = "tree root of Map ("
    <> show (typeOf (error "shouldn't fire" :: h))
    <> ") ("
    <> show (typeOf (error "shouldn't fire" :: k))
    <> ") ("
    <> show (typeOf (error "shouldn't fire" :: v))
    <> ")"

-- | 'Maybe' is there due to SD imposing 'Default' constrains
--   on accum /because reasons/.
type AVLChgAccum h k v = Maybe (RealAVLAccum h k v)

data RealAVLAccum h k v =
  RealAVLAccum
    { acaMap     :: AVL.Map h k v
    -- ^ AVL map, which contains avl tree with most-recent updates
    , acaTouched :: Set AVL.Revision
    -- ^ Set of nodes, which were touched during all of change operations applied on tree
    }

-- | If accumulator is not initialised, read current root and make a new
--   shiny accumulator out of it.
initAccum :: AVLMutates h k v m => DB.Plugin -> AVLChgAccum h k v -> m (RealAVLAccum h k v)
initAccum plugin Nothing  = accumFromAVLMap <$> (AVL.currentRoot `runReaderT` plugin)
initAccum _     (Just it) = return it

accumFromAVLMap :: AVL.Map h k v -> RealAVLAccum h k v
accumFromAVLMap tree = RealAVLAccum tree Set.empty

initAVLStorage :: forall h k v m . AVLMutates h k v m => DB.Plugin -> Map k v -> m ()
initAVLStorage plugin kvs = do
    AVL.initialiseStorageIfNotAlready @h (Map.toList kvs) `runReaderT` plugin

-- | DbModifyActions with given plugin - rdb one or pure.
mkAvlDbModifyActions
  :: forall h k v m n
  .  ( MonadIO n
     , IdSumPrefixed k
     , AVLMutates h k v m
     )
  => DB.Plugin
  -> n (DbModifyActions (AVLChgAccum h k v) k v m (AVL.Proof h k v))
mkAvlDbModifyActions plugin = do
    lookups <- newTVarIO (Set.empty :: Set AVL.Revision)
    return DbModifyActions
        { dmaAccessActions = mkAvlDbAccessActions plugin lookups
        , dmaApply         = mkDmaApply                  lookups
        }
  where
    run :: ReaderT DB.Plugin m a -> m a
    run = flip runReaderT plugin

    -- Apply changes to the database.
    mkDmaApply :: TVar (Set AVL.Revision) -> AVLChgAccum h k v -> m (AVL.Proof h k v)
    mkDmaApply lookups accum' = do
      accum        <- initAccum plugin accum'
      nodesTouched <- atomically $ STM.swapTVar lookups Set.empty
      old          <- run AVL.currentRoot
      let preproof =  nodesTouched <> acaTouched accum
      proof        <- run $ AVL.prune preproof (AVL.fullRehash old)

      -- To prevent unnessessary growth of storage,
      -- we will 'overwrite' for now instead of 'append'ing.
      run $ AVL.overwrite (acaMap accum)
      return proof

-- | Readonly interface for given plugin.
mkAvlDbAccessActions
  :: forall h k v m
  .  ( IdSumPrefixed k
     , AVLMutates h k v m
     )
  => DB.Plugin
  -> TVar (Set.Set AVL.Revision)
  -> DbAccessActions (AVLChgAccum h k v) k v m
mkAvlDbAccessActions plugin lookups = DbAccessActions
    { daaGetter
    , daaModifyAccum
    , daaIter
    }
  where
    run :: ReaderT DB.Plugin m a -> m a
    run = flip runReaderT plugin

    storeNodeset :: Set.Set AVL.Revision -> m ()
    storeNodeset nodeset = do
        atomically $ STM.modifyTVar lookups (<> nodeset)

    daaGetter :: AVLChgAccum h k v -> StateR k -> m (StateP k v)
    daaGetter accum' req = do
        accum <- initAccum plugin accum'
        let
          getOne newAccum key = do
            ((val, proof), _) <- run $ AVL.lookup key (acaMap newAccum)
            storeNodeset proof
            return $ maybe Map.empty (Map.singleton key) val

        ss <- mapM (getOne accum) (Set.toList req)
        return $ Map.unions ss

    daaIter
        :: forall b
        .  AVLChgAccum h k v
        -> Prefix
        -> b
        -> ((k, v) -> b -> b)
        -> m b
    daaIter accum' prefix start folder = do
        accum <- initAccum plugin accum'
        (result, nodeset) <- run $ AVL.foldIf (\x -> idSumPrefix x == prefix, start, folder, id) (acaMap accum)
        storeNodeset nodeset
        return result

    daaModifyAccum
        :: AVLChgAccum h k v
        -> ChgAccumModifier k v
        -> m (Either (CSMappendException k) (AVLChgAccum h k v, Undo k v))
    daaModifyAccum oldAccum changes = do
        let cSet = case changes of
              CAMChange       cs          -> cs
              CAMRevert (Undo cs _unused) -> cs

        applyChangeSet plugin cSet oldAccum

-- | Apply changes to the accum, using the storage plugin.
applySingleChange
    :: forall h k v m
    .  ( IdSumPrefixed k
       , AVLMutates h k v m
       )
    => DB.Plugin -> RealAVLAccum h k v -> (k, ValueOp v)  -> m (RealAVLAccum h k v)
applySingleChange plugin accum (k, dv) = do
    ((mres, _), _) <- run $ AVL.lookup k (acaMap accum)
    case (dv, mres) of
      (NotExisted, _)      -> return accum
      (New thing, Nothing) -> accum `withAccum` AVL.insert k thing
      (Upd thing, Just _)  -> accum `withAccum` AVL.insert k thing
      (Rem,       Just _)  -> accum `withAccum` AVL.delete k
      _                    -> throwM $ CSMappendException k
  `catch` \(_ :: AVL.NotFound h) ->
    throwM $ CSMappendException k
  where
    run :: ReaderT DB.Plugin m a -> m a
    run = flip runReaderT plugin

    withAccum accum' action = do
        (proof, map') <- run $ action (acaMap accum')
        return accum'
            { acaMap     = map'
            , acaTouched = acaTouched accum' <> proof
            }

-- | Generate "undo".
computeUndo
    :: forall h k v m
    .  ( IdSumPrefixed k
       , AVLMutates h k v m
       )
    => DB.Plugin -> RealAVLAccum h k v -> (k, ValueOp v) -> m (k, ValueOp v)
computeUndo plugin accum (k, dv) = do
    ((old, _), _) <- run $ AVL.lookup k (acaMap accum)
    case (dv, old) of
      (New _,      Nothing)  -> return (k, Rem)
      (NotExisted, Nothing)  -> return (k, NotExisted)
      (Upd _,      Just eld) -> return (k, Upd eld)
      (Rem,        Just eld) -> return (k, New eld)
      _other                 -> throwM $ CSMappendException k
  where
    run :: ReaderT DB.Plugin m a -> m a
    run = flip runReaderT plugin

-- | Using plugin, apply set of changes (and generate undo).
applyChangeSet
    :: forall h k v m
    .  ( IdSumPrefixed k
       , AVLMutates h k v m
       )
    => DB.Plugin
    -> ChangeSet k v
    -> AVLChgAccum h k v
    -> m (Either (CSMappendException k) (AVLChgAccum h k v, Undo k v))
applyChangeSet plugin (changeSetToList -> csList) accum' = do
    accum <- initAccum plugin accum'
    undo  <- mapM  (computeUndo plugin accum) csList
    res   <- foldM (applySingleChange plugin) accum csList

    let undoCS = ChangeSet (Map.fromList undo)

    return (Right (Just res, Undo undoCS BS.empty))
  `catch` \(e :: CSMappendException k) ->
    return (Left e)
