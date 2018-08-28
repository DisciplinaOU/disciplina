
module Dscp.DB.CanProvideDB.Pure (PureStateVar, newCtxVar, plugin) where

import Codec.Serialise
import qualified Data.Map as Map

import Dscp.DB.CanProvideDB.Class as Abstract
import Dscp.DB.CanProvideDB.Common
import Dscp.Util.Serialise (serialise')

type PureStateVar = TVar PureCtx

newCtxVar :: IO PureStateVar
newCtxVar = do
    newTVarIO $ PureCtx Map.empty Map.empty

data PureCtx = PureCtx
    { noPrefix :: Storage
    , prefixed :: Map.Map Prefix Storage'
    }

type Storage  = Map.Map ByteString ByteString
type Storage' = Map.Map Ids        Values

ro :: MonadIO m => PureStateVar -> (Storage -> Maybe a) -> m (Maybe a)
ro var action =
    atomically $ action . noPrefix <$> readTVar var

roPrefix :: MonadIO m => PureStateVar -> Prefix -> (Storage' -> Maybe a) -> m (Maybe a)
roPrefix var prefix action = do
    mmap <- atomically $ prefixed <$> readTVar var
    return $ do
        slice <- Map.lookup prefix mmap
        action slice

rw :: MonadIO m => PureStateVar -> (Storage -> Storage) -> m ()
rw var action =
    atomically $ modifyTVar' var $ \s ->
        s { noPrefix = action $ noPrefix s }

rwPrefix :: MonadIO m => PureStateVar -> Prefix -> (Storage' -> Storage') -> m ()
rwPrefix var prefix action =
    atomically $ modifyTVar' var $ \s ->
        s { prefixed =
                Map.alter
                    (return . action . fromMaybe Map.empty)
                    prefix
                    (prefixed s)
          }

plugin :: PureStateVar -> Plugin
plugin var = Plugin
    { pGet = \k ->
        ro var $ \store ->
            deserialiseErr "Failed to deserialise"
            <$>  Map.lookup (serialise' k) store

    , pBatch = \ops -> do
        rw var $ \st -> foldl (flip act) st ops
        return ()

    , pGetWithPrefix = \prefix k -> do
        roPrefix var prefix $ \store ->
            Map.lookup k store

    , pDoWithPrefix = \prefix op ->
        rwPrefix var prefix $ act' op

    , pIterate = \prefix start folder -> do
        lst <- roPrefix var prefix $ return . Map.toList
        return $ foldl folder start $ fromMaybe [] lst
    }
  where
    act :: (Serialise k, Serialise v) => Operation k v -> Storage -> Storage
    act = \case
        Abstract.Put    k v -> Map.insert (serialise' k) (serialise' v)
        Abstract.Delete k   -> Map.delete (serialise' k)

    act' :: Operation Ids Values -> Storage' -> Storage'
    act' = \case
        Abstract.Put    k v -> Map.insert k v
        Abstract.Delete k   -> Map.delete k
