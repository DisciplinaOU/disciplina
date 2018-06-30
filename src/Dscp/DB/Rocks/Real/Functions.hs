{-# LANGUAGE TypeApplications #-}

module Dscp.DB.Rocks.Real.Functions
       ( -- * Closing/opening
         openRocksDB
       , closeRocksDB
       , openNodeDB
       , closeNodeDB
         -- * Reading/writing
       , rocksGetBytes
       , rocksPutBytes
       , rocksDelete
       ) where

import qualified Database.RocksDB as Rocks
import Loot.Base.HasLens (HasLens (..), HasLens')

import Dscp.DB.Rocks.Class (MonadDB (..), MonadDBRead (..))
import Dscp.DB.Rocks.Real.Types (DB (..), MonadRealDB, RocksDB (..), RocksDBParams (..), rdDatabase)

-----------------------------------------------------------
-- Opening/closing
-----------------------------------------------------------

openRocksDB :: MonadIO m => FilePath -> m DB
openRocksDB path = do
    let rocksReadOpts = Rocks.defaultReadOptions
        rocksWriteOpts = Rocks.defaultWriteOptions
        rocksOptions = Rocks.defaultOptions
            { Rocks.createIfMissing = True
            , Rocks.compression = Rocks.NoCompression
            }
    rocksDB <- Rocks.open path rocksOptions
    return DB {..}

closeRocksDB :: MonadIO m => DB -> m ()
closeRocksDB = Rocks.close . rocksDB

openNodeDB :: MonadIO m => RocksDBParams -> m RocksDB
openNodeDB RocksDBParams{..} = RocksDB <$> openRocksDB rdpPath

closeNodeDB :: MonadIO m => RocksDB -> m ()
closeNodeDB = closeRocksDB . _rdDatabase

------------------------------------------------------------
-- Reading/writing
------------------------------------------------------------

-- | Read ByteString from RocksDb using given key.
rocksGetBytes :: MonadIO m => ByteString -> DB -> m (Maybe ByteString)
rocksGetBytes key DB {..} = Rocks.get rocksDB rocksReadOpts key

-- | Write ByteString to RocksDB for given key.
rocksPutBytes :: MonadIO m => ByteString -> ByteString -> DB -> m ()
rocksPutBytes k v DB {..} = Rocks.put rocksDB rocksWriteOpts k v

-- | Delete element from RocksDB for given key.
rocksDelete :: MonadIO m => ByteString -> DB -> m ()
rocksDelete k DB {..} = Rocks.delete rocksDB rocksWriteOpts k

------------------------------------------------------------
-- Instances
------------------------------------------------------------

getDB :: (MonadReader ctx m, HasLens' ctx RocksDB) => m DB
getDB = view $ lensOf @RocksDB . rdDatabase

instance MonadRealDB ctx m => MonadDBRead m where
    dbGet key = getDB >>= rocksGetBytes key

instance MonadRealDB ctx m => MonadDB m where
    dbPut key val = getDB >>= rocksPutBytes key val
    dbDelete key = getDB >>= rocksDelete key
