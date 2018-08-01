{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

module Dscp.Snowdrop.Storage.Pure where

import Control.Concurrent.STM.TVar (modifyTVar)
import Control.Lens (at, makeLenses)
import qualified Data.Map as M

import Snowdrop.Model.Block (BlockRef (..), TipKey (..), TipValue (..))
import Snowdrop.Model.Execution (DbActionsException (..), DbModifyActions (..), SumChangeSet,
                                 accumToDiff, sumChangeSetDBA)
import Snowdrop.Model.State.Core (HasKeyValue, SValue)
import Snowdrop.Util

import Dscp.Core.Foundation (HeaderHash)
import Dscp.Snowdrop.AccountValidation (Account, AccountId)
import Dscp.Snowdrop.Configuration (Ids (..), SBlund, Values (..), accountPrefix, blockPrefix,
                                    tipPrefix)

data BlockStorage = BlockStorage
    { _bsBlunds :: Map HeaderHash SBlund
    , _bsTip    :: TipValue HeaderHash
    }
makeLenses ''BlockStorage

type AddrMap = Map AccountId Account

data StateStorage = StateStorage
    { _ssAddrMap        :: AddrMap
    }
makeLenses ''StateStorage

emptyBlockStorage :: BlockStorage
emptyBlockStorage = BlockStorage mempty (TipValue Nothing)

queryMany :: (Ord id, Monad m, Foldable f) => (id -> m (Maybe value)) -> f id -> m (Map id value)
queryMany doOne = foldM (\resp i -> maybe resp (\v -> M.insert i v resp) <$> doOne i) mempty

_getterLogger
  :: (Show accum, Show req, Show resp, MonadIO m)
  => (accum -> req -> m resp) -> accum -> req -> m resp
_getterLogger impl accum reqIds = do
    putTextLn $ "Getter: accum: " <> show accum <> ", req: " <> show reqIds
    resp <- impl accum reqIds
    putTextLn $ "   getter: response " <> show resp
    pure resp

blockDbActions ::
       (MonadIO m, MonadIO n)
    => m (DbModifyActions (SumChangeSet Ids Values) Ids Values n ())
blockDbActions = mkActions <$> liftIO (newTVarIO emptyBlockStorage)
  where
    mkActions var =
        DbModifyActions (mkAccessActions var) (atomically . apply var)
    mkAccessActions var =
        sumChangeSetDBA (atomically ... queryMany $ queryOne var) (atomically ... iter var)

    iter :: TVar BlockStorage -> Prefix -> b -> ((Ids, Values) -> b -> b) -> STM b
    iter var prefix b foldF
      | prefix == tipPrefix =
        foldr foldF b . (\tip -> [(TipKeyIds TipKey, TipValueVal tip)]) . _bsTip <$> readTVar var
      | prefix == blockPrefix =
        foldr foldF b . map (bimap (BlockRefIds . BlockRef) BlundVal) . M.toList . _bsBlunds <$> readTVar var
      | otherwise =
        throwM $ DbWrongPrefixIter $ "Unknown iteration on block storage " <> show prefix

    queryOne :: TVar BlockStorage -> Ids -> STM (Maybe Values)
    queryOne var = \case
        (TipKeyIds TipKey)          -> Just . TipValueVal . _bsTip <$> readTVar var
        (BlockRefIds (BlockRef hh)) -> fmap BlundVal . M.lookup hh . _bsBlunds <$> readTVar var
        i -> throwM $ DbWrongIdQuery $ "Unknown query to block storage " <> show i

    apply :: TVar BlockStorage -> SumChangeSet Ids Values -> STM ()
    apply var accum = mapM_ (uncurry $ applyOne var) $ changeSetToList $ accumToDiff accum

    applyOne :: TVar BlockStorage -> Ids -> ValueOp Values -> STM ()
    applyOne var (TipKeyIds TipKey) =
        \valueop -> unTipValue . _bsTip <$> readTVar var >>= applyTip var valueop
    applyOne var (BlockRefIds (BlockRef hh)) =
        performActionWithTVar var (bsBlunds . at hh) (applyException hh) <=<
        projValOp @(BlockRef HeaderHash)
    applyOne _ i = applyException i '-'

    applyTip :: TVar BlockStorage -> ValueOp Values -> Maybe HeaderHash -> STM ()
    applyTip var (Upd (TipValueVal newTip)) _ = setTip var newTip
    applyTip _   vOp val                      = applyException TipKey vOp val

    setTip var newTip = modifyTVar var (\bs -> bs { _bsTip = newTip })

simpleStateDbActions ::
       (MonadIO m, MonadIO n)
    => AddrMap
    -> m (DbModifyActions (SumChangeSet Ids Values) Ids Values n ())
simpleStateDbActions initAddrMap =
    mkActions <$> newTVarIO (StateStorage initAddrMap)
  where
    mkActions var =
        DbModifyActions (mkAccessActions var) (atomically . apply var)
    mkAccessActions var =
        sumChangeSetDBA (atomically ... queryMany (queryOne var)) (atomically ... iter var)

    -- Queries
    queryOne :: TVar StateStorage -> Ids -> STM (Maybe Values)
    queryOne tvar = \case
        AccountInIds a -> fmap AccountOutVal . M.lookup a . _ssAddrMap <$> readTVar tvar
        i -> throwM $ DbWrongIdQuery $ "Unknown query to state " <> show i

    iter :: TVar StateStorage -> Prefix -> b -> ((Ids, Values) -> b -> b) -> STM b
    iter var prefix
        | prefix == accountPrefix =
          iterHelper AccountInIds AccountOutVal (_ssAddrMap <$> readTVar var)
        | otherwise =
          const $ const $ throwM $
          DbWrongPrefixIter $ "Unknown iteration on state " <> show prefix

    -- Modifies
    apply :: TVar StateStorage -> SumChangeSet Ids Values -> STM ()
    apply vars accum = mapM_ (uncurry $ applyOne vars) $ changeSetToList $ accumToDiff accum

    applyOne :: TVar StateStorage -> Ids -> ValueOp Values -> STM ()
    applyOne var (AccountInIds a) =
        performActionWithTVar var (ssAddrMap . at a) (applyException a) <=<
        projValOp @AccountId
    applyOne _ i = applyException i '-'

    iterHelper :: (id' -> Ids)
               -> (val' -> Values)
               -> STM (Map id' val')
               -> b
               -> ((Ids, Values) -> b -> b)
               -> STM b
    iterHelper injId injVal var b foldF =
        foldr foldF b . map (bimap injId injVal) . M.toList <$> var


----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

applyException :: (Show key, Show vOp, Show val, MonadThrow m) => key -> val -> vOp -> m void
applyException key val vOp =
    throwM $ DbApplyException $
    "Error applying operation " <> show vOp <> " to value " <>
    show val <> " (key: " <> show key <> ")"

data ValueTypeMismatched value = ValueTypeMismatched (ValueOp Values) (Proxy value)
    deriving (Show)
instance Typeable value => Exception (ValueTypeMismatched value)

projValOp
    :: forall id value m . (MonadThrow m, HasKeyValue Ids Values id value, Typeable value)
    => ValueOp Values -> m (ValueOp value)
projValOp valueop = case sequenceA $ proj @Values @(SValue id) <$> valueop of
    Nothing -> throwM $ ValueTypeMismatched valueop (Proxy @value)
    Just v  -> pure v

performActionWithMap ::
       (Show id, Show value, Ord id)
    => TVar (Map id value)
    -> id
    -> ValueOp value
    -> STM ()
performActionWithMap tvar i = performActionWithTVar tvar (at i) (applyException i)

performActionWithTVar
    :: TVar var
    -> Lens' var (Maybe value)
    -> (ValueOp value -> Maybe value -> STM ())
    -> ValueOp value
    -> STM ()
performActionWithTVar tvar ln onEx valop = (valop,) . view ln <$> readTVar tvar >>= \case
    (Rem, Just _)         -> setVal Nothing
    (New v, Nothing)      -> setVal (Just v)
    (Upd v, Just _)       -> setVal (Just v)
    (NotExisted, Nothing) -> pure ()
    (_, val)              -> onEx valop val
  where
    setVal = modifyTVar tvar . set ln
