{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

module Dscp.Snowdrop.Storage.Pure
    ( blockDbActions
    ) where

import Control.Concurrent.STM.TVar (modifyTVar)
import Control.Lens (at, makeLenses)
import qualified Data.Map as M

import Snowdrop.Block (BlockRef (..), TipKey (..), TipValue (..))
import Snowdrop.Core (HasKeyValue, Prefix, SValue, ValueOp (..), changeSetToList)
import Snowdrop.Execution (DbActionsException (..), DbModifyActions (..), SumChangeSet, accumToDiff,
                           sumChangeSetDBA)
import Snowdrop.Util

import Dscp.Core.Foundation
import Dscp.Snowdrop.Configuration
import Dscp.Snowdrop.Storage.Types

data BlockStorage = BlockStorage
    { _bsBlunds        :: Map HeaderHash SBlund
    , _bsTip           :: TipValue HeaderHash
    , _bsTxs           :: Map GTxId TxBlockRef
    , _bsAccLastTxs    :: Map TxsOf LastTx
    , _bsAccNextTxs    :: Map TxHead TxNext
    , _bsPrivBlockPubs :: Map PrivateHeaderHash PublicationTxId
    , _bsAccPubs       :: Map PublicationTxId PublicationData
    , _bsAccLastPubs   :: Map PublicationsOf LastPublication
    , _bsAccNextPubs   :: Map PublicationHead PublicationNext
    , _bsAccPubBlocks  :: Map PublicationBlock PublicationBlockRef
    , _bsNextBlocks    :: Map NextBlockOf NextBlock
    , _bsBlockIdx      :: Map Difficulty HeaderHash
    }
makeLenses ''BlockStorage

emptyBlockStorage :: BlockStorage
emptyBlockStorage = BlockStorage mempty (TipValue Nothing) mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

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
       (MonadIO m, MonadIO n, MonadCatch n)
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
      | prefix == txPrefix =
        foldr foldF b . map (bimap TxIds TxVal) . M.toList . _bsTxs <$> readTVar var
      | prefix == txOfPrefix =
        foldr foldF b . map (bimap TxOfIds TxOfVal) . M.toList . _bsAccLastTxs <$> readTVar var
      | prefix == txHeadPrefix =
        foldr foldF b . map (bimap TxHeadIds TxHeadVal) . M.toList . _bsAccNextTxs <$> readTVar var
      | prefix == privateBlockTxPrefix =
        foldr foldF b . map (bimap PrivateBlockTx PrivateBlockTxVal) . M.toList . _bsPrivBlockPubs <$> readTVar var
      | prefix == publicationIdsPrefix =
        foldr foldF b . map (bimap PublicationIds PublicationVal) . M.toList . _bsAccPubs <$> readTVar var
      | prefix == publicationOfPrefix =
        foldr foldF b . map (bimap PublicationOfIds PublicationOfVal) . M.toList . _bsAccLastPubs <$> readTVar var
      | prefix == publicationHeadPrefix =
        foldr foldF b . map (bimap PublicationHeadIds PublicationHeadVal) . M.toList . _bsAccNextPubs <$> readTVar var
      | prefix == publicationBlockIdsPrefix =
        foldr foldF b . map (bimap PublicationBlockIds PublicationBlockVal) . M.toList . _bsAccPubBlocks <$> readTVar var
      | prefix == nextBlockPrefix =
        foldr foldF b . map (bimap NextBlockOfIds NextBlockOfVal) . M.toList . _bsNextBlocks <$> readTVar var
      | prefix == blockIdxPrefix =
        foldr foldF b . map (bimap BlockIdxIds BlockIdxVal) . M.toList . _bsBlockIdx <$> readTVar var
      | otherwise =
        throwM $ DbWrongPrefixIter $ "Unknown iteration on block storage " <> show prefix

    queryOne :: TVar BlockStorage -> Ids -> STM (Maybe Values)
    queryOne var = \case
        (TipKeyIds TipKey)           -> Just . TipValueVal . view bsTip <$> readTVar var
        (BlockRefIds (BlockRef hh))  -> fmap BlundVal . M.lookup hh . view bsBlunds <$> readTVar var
        (TxIds gTxId)                -> fmap TxVal . M.lookup gTxId . view bsTxs <$> readTVar var
        (TxOfIds txOf)               -> fmap TxOfVal . M.lookup txOf . view bsAccLastTxs <$> readTVar var
        (TxHeadIds txHead)           -> fmap TxHeadVal . M.lookup txHead . view bsAccNextTxs <$> readTVar var
        (PrivateBlockTx pbh)         -> fmap PrivateBlockTxVal . M.lookup pbh . view bsPrivBlockPubs <$> readTVar var
        (PublicationIds ptxId)       -> fmap PublicationVal . M.lookup ptxId . view bsAccPubs <$> readTVar var
        (PublicationOfIds pubOf)     -> fmap PublicationOfVal . M.lookup pubOf . view bsAccLastPubs <$> readTVar var
        (PublicationHeadIds pubHead) -> fmap PublicationHeadVal . M.lookup pubHead . view bsAccNextPubs <$> readTVar var
        (PublicationBlockIds pbh)    -> fmap PublicationBlockVal . M.lookup pbh . view bsAccPubBlocks <$> readTVar var
        (NextBlockOfIds hh)          -> fmap NextBlockOfVal . M.lookup hh . view bsNextBlocks <$> readTVar var
        (BlockIdxIds d)              -> fmap BlockIdxVal . M.lookup d . view bsBlockIdx <$> readTVar var
        i -> throwM $ DbWrongIdQuery $ "Unknown query to block storage " <> show i

    apply :: TVar BlockStorage -> SumChangeSet Ids Values -> STM ()
    apply var accum = mapM_ (uncurry $ applyOne var) $ changeSetToList $ accumToDiff accum

    applyOne :: TVar BlockStorage -> Ids -> ValueOp Values -> STM ()
    applyOne var (TipKeyIds TipKey) =
        \valueop -> unTipValue . view bsTip <$> readTVar var >>= applyTip var valueop
    applyOne var (BlockRefIds (BlockRef hh)) =
        performActionWithTVar var (bsBlunds . at hh) (applyException hh) <=<
        projValOp @(BlockRef HeaderHash)
    applyOne var (TxIds gTxId) =
        performActionWithTVar var (bsTxs . at gTxId) (applyException gTxId) <=<
        projValOp @GTxId
    applyOne var (TxOfIds txOf) =
        performActionWithTVar var (bsAccLastTxs . at txOf) (applyException txOf) <=<
        projValOp @TxsOf
    applyOne var (TxHeadIds txHead) =
        performActionWithTVar var (bsAccNextTxs . at txHead) (applyException txHead) <=<
        projValOp @TxHead
    applyOne var (PrivateBlockTx pbh) =
        performActionWithTVar var (bsPrivBlockPubs . at pbh) (applyException pbh) <=<
        projValOp @PrivateHeaderHash
    applyOne var (PublicationIds ptxId) =
        performActionWithTVar var (bsAccPubs . at ptxId) (applyException ptxId) <=<
        projValOp @PublicationTxId
    applyOne var (PublicationOfIds pubOf) =
        performActionWithTVar var (bsAccLastPubs . at pubOf) (applyException pubOf) <=<
        projValOp @PublicationsOf
    applyOne var (PublicationHeadIds pubHead) =
        performActionWithTVar var (bsAccNextPubs . at pubHead) (applyException pubHead) <=<
        projValOp @PublicationHead
    applyOne var (PublicationBlockIds pbh) =
        performActionWithTVar var (bsAccPubBlocks . at pbh) (applyException pbh) <=<
        projValOp @PublicationBlock
    applyOne var (NextBlockOfIds hh) =
        performActionWithTVar var (bsNextBlocks . at hh) (applyException hh) <=<
        projValOp @NextBlockOf
    applyOne var (BlockIdxIds d) =
        performActionWithTVar var (bsBlockIdx . at d) (applyException d) <=<
        projValOp @Difficulty
    applyOne _ i = applyException i '-'

    applyTip :: TVar BlockStorage -> ValueOp Values -> Maybe HeaderHash -> STM ()
    applyTip var (Upd (TipValueVal newTip)) _ = setTip var newTip
    applyTip _   vOp val                      = applyException TipKey vOp val

    setTip var newTip = modifyTVar var (\bs -> bs { _bsTip = newTip })


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
