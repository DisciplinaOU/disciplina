-- | Plugin-based storage adapter for SD.

module Dscp.Snowdrop.Storage.PluginBased (blockActions) where

import qualified Data.Map as M
import UnliftIO (MonadUnliftIO)

import Snowdrop.Block (TipValue (..))
import Snowdrop.Core (Prefix (..), ValueOp (..), changeSetToList)
import Snowdrop.Execution (DbModifyActions (..), SumChangeSet (..), sumChangeSetDBA)

import qualified Dscp.DB.CanProvideDB as DB
import Dscp.Snowdrop.Configuration
import Dscp.Snowdrop.Serialise ()

blockActions :: forall m n.
       (MonadIO m, MonadUnliftIO n, MonadCatch n)
    => DB.Plugin -> m (DbModifyActions (SumChangeSet Ids Values) Ids Values n ())
blockActions plugin =
    pure $ DbModifyActions (sumChangeSetDBA (queryMany getImpl) iterImpl) applyImpl
  where

    queryMany doOne = foldM (\resp i -> maybe resp (\v -> M.insert i v resp) <$> doOne i) mempty

    getImpl :: Ids -> n (Maybe Values)
    getImpl k = do
        mvalue <- DB.pGet plugin k
        case k of
            -- This seems renudant, as now I check that Tip exists,
            -- before applying genesis. The latter application does nothing
            -- to Tip, so I have to deal with it.

            TipKeyIds _ ->
                case mvalue of
                    Nothing -> pure $ Just $ TipValueVal $ TipValue Nothing
                    x       -> pure x
            _ ->
                return mvalue

    iterImpl :: Prefix -> b -> ((Ids, Values) -> b -> b) -> n b
    iterImpl (Prefix prefix) accum folder =
        DB.pIterate plugin prefix accum (flip folder)

    applyImpl :: SumChangeSet Ids Values -> n ()
    applyImpl cs =
        DB.pBatch plugin $ map toBatchOp $ changeSetToList $ unSumCS cs
      where
        toBatchOp (k, Rem)        = DB.Delete k
        toBatchOp (k, Upd v)      = DB.Put    k v
        toBatchOp (k, New v)      = DB.Put    k v
        toBatchOp (_, NotExisted) = error "PluginBased.blockActions.toBatchOp: NotExisted not expected"
