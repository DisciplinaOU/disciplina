{-# LANGUAGE TypeApplications #-}

module Dscp.DB.Rocks.Real.Functions
       (

         -- * Carrier constraint
         HasRocksDB

         -- * Closing/opening
       , openRocksDB
       , closeRocksDB
       , openNodeDB
       , closeNodeDB

         -- * Reading/writing
       -- , rocksGetBytes
       -- , rocksPutBytes
       -- , rocksDelete
       -- , rocksWriteBatch

       -- , getDB

         -- * More high-level interface
       , DeserialisationError
       , get
       , getWith
       , writeBatch
       , iterate
       ) where

import Prelude hiding (get, iterate, put)

import Codec.Serialise
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Database.RocksDB as Rocks
import Loot.Base.HasLens (HasLens)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import UnliftIO (MonadUnliftIO)

import Dscp.DB.Rocks.Real.Types (DB (..), RocksDB (..), RocksDBParams (..))
import Dscp.Util
import Dscp.Util.Serialise

-----------------------------------------------------------
-- Opening/closing
-----------------------------------------------------------

data DeserialisationError = DeserialisationError DeserialiseFailure
    deriving (Show, Typeable)

instance Exception DeserialisationError

type HasRocksDB ctx m = (MonadReader ctx m, HasLens ctx RocksDB, MonadIO m, MonadThrow m)

openRocksDB :: MonadIO m => FilePath -> m DB
openRocksDB path = do
    let rocksReadOpts  = Rocks.defaultReadOptions
        rocksWriteOpts = Rocks.defaultWriteOptions
        rocksOptions   = Rocks.defaultOptions
            { Rocks.createIfMissing = True
            , Rocks.compression     = Rocks.NoCompression
            }
    rocksDB <- Rocks.open path rocksOptions
    return DB
        { rocksReadOpts
        , rocksWriteOpts
        , rocksOptions
        , rocksDB
        }

closeRocksDB :: MonadIO m => DB -> m ()
closeRocksDB = Rocks.close . rocksDB

openNodeDB :: MonadIO m => RocksDBParams -> m RocksDB
openNodeDB RocksDBParams{..} = liftIO $ do
    dirExists <- doesDirectoryExist rdpPath

    when (rdpClean && dirExists) $ do
        removeDirectoryRecursive rdpPath

    RocksDB <$> openRocksDB rdpPath

closeNodeDB :: MonadIO m => RocksDB -> m ()
closeNodeDB = closeRocksDB . _rdDatabase

------------------------------------------------------------
-- Reading/writing
------------------------------------------------------------

get :: (Serialise a, Serialise b, MonadThrow m, MonadIO m) => DB -> a -> m (Maybe b)
get db = getWith db serialise'

getWith :: (Serialise b, MonadThrow m, MonadIO m) => DB -> (a -> ByteString) -> a -> m (Maybe b)
getWith db project a = do
    traverse (leftToThrow DeserialisationError . deserialiseOrFail')
        =<< rocksGetBytes db (project a)
  where
    -- | Read ByteString from RocksDb using given key.
    rocksGetBytes :: MonadIO m => DB -> ByteString -> m (Maybe ByteString)
    rocksGetBytes DB {..} key = Rocks.get rocksDB rocksReadOpts key

-- | Write a batch.
writeBatch :: MonadIO m => DB -> [Rocks.BatchOp] -> m ()
writeBatch DB {..} = Rocks.write rocksDB rocksWriteOpts

-- | Iterator implemented as a left fold.
iterate ::
       (MonadIO m, MonadUnliftIO m)
    => DB
    -> ByteString
    -> b
    -> (b -> (ByteString, ByteString) -> b)
    -> m b
iterate DB {..} prefix f0 foldF =
    runResourceT $
    Rocks.withIterator rocksDB rocksReadOpts $ \iter -> do
        Rocks.iterSeek iter prefix
        let process acc =
                Rocks.iterEntry iter >>= \case
                    Nothing -> pure acc
                    Just e -> do Rocks.iterNext iter
                                 process (foldF acc e)
        process f0
