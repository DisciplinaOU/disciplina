module Dscp.DB.Rocks.Real.Types
       ( MonadRealDB
       , DB (..)
       , RocksDBParams (..)
       , RocksDB (..)
       , rdDatabase
       ) where

import Universum

import Control.Lens (makeLenses)
import qualified Database.RocksDB as Rocks
import Loot.Base.HasLens (HasLens')

-- | Set of constraints necessary to operate on real DB.
type MonadRealDB ctx m =
    ( MonadReader ctx m
    , HasLens' ctx RocksDB
    , MonadIO m
    , Monad m
    )

-- | Internal helper to carry Rocks parameters and handlers.
data DB = DB
    { rocksReadOpts  :: !Rocks.ReadOptions
    , rocksWriteOpts :: !Rocks.WriteOptions
    , rocksOptions   :: !Rocks.Options
    , rocksDB        :: !Rocks.DB
    }

-- | Set of parameters provided on opening connection.
data RocksDBParams = RocksDBParams
    { rdpPath :: !FilePath
    -- ^ Path to the database
    } deriving Show

data RocksDB = RocksDB
    { _rdDatabase :: !DB
    }

makeLenses ''RocksDB
