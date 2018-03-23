module Disciplina.DB.Functions
       ( openRocksDB
       , closeRocksDB
       , openNodeDB
       , closeNodeDB
       ) where

import Universum

import qualified Database.RocksDB as Rocks

import Disciplina.DB.Types (DB (..), DBType, NodeDB (..))

openRocksDB :: MonadIO m => FilePath -> m DB
openRocksDB path = do
    let rocksReadOpts = Rocks.defaultReadOptions
        rocksWriteOpts = Rocks.defaultWriteOptions
        rocksOptions = (Rocks.defaultOptions path)
            { Rocks.optionsCreateIfMissing = True
            , Rocks.optionsCompression = Rocks.NoCompression
            }
    rocksDB <- Rocks.open rocksOptions
    return DB {..}

closeRocksDB :: MonadIO m => DB -> m ()
closeRocksDB = Rocks.close . rocksDB

openNodeDB :: MonadIO m => DBType -> FilePath -> m NodeDB
openNodeDB dbType path = NodeDB dbType <$> openRocksDB path

closeNodeDB :: MonadIO m => NodeDB -> m ()
closeNodeDB = closeRocksDB . _ndbDatabase
