{-# LANGUAGE TypeApplications #-}

module Disciplina.DB.Real.Functions
       ( -- * Closing/opening
         openRocksDB
       , closeRocksDB
       , openNodeDB
       , closeNodeDB
       , bracketNodeDB
         -- * Reading/writing
       , rocksGetBytes
       , rocksPutBytes
       , rocksDelete
       ) where

import Universum

import qualified Database.RocksDB as Rocks
import Ether.Internal (HasLens (..))

import Disciplina.DB.Class (MonadDB (..), MonadDBRead (..))
import Disciplina.DB.Real.Types (DB (..), DBParams (..), MonadRealDB, NodeDB (..), ndbDatabase)

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

openNodeDB :: MonadIO m => DBParams -> m NodeDB
openNodeDB DBParams{..} = NodeDB <$> openRocksDB dbpPath

closeNodeDB :: MonadIO m => NodeDB -> m ()
closeNodeDB = closeRocksDB . _ndbDatabase

bracketNodeDB
    :: (MonadIO m, MonadMask m)
    => DBParams -> (NodeDB -> m a) -> m a
bracketNodeDB params = bracket (openNodeDB params) closeNodeDB

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

getDB :: (MonadReader ctx m, HasLens NodeDB ctx NodeDB) => m DB
getDB = view $ lensOf @NodeDB . ndbDatabase

instance MonadRealDB ctx m => MonadDBRead m where
    dbGet key = getDB >>= rocksGetBytes key

instance MonadRealDB ctx m => MonadDB m where
    dbPut key val = getDB >>= rocksPutBytes key val
    dbDelete key = getDB >>= rocksDelete key
