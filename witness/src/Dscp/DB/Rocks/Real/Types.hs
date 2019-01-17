module Dscp.DB.Rocks.Real.Types
       ( MonadRealDB
       , DB (..)
       , RocksDBParams
       , RocksDBParamsRec
       , RocksDBParamsRecP
       , RocksDB (..)
       , rdDatabase
       , Rocks.BatchOp (..)
       ) where

import Control.Lens (makeLenses)
import qualified Database.RocksDB as Rocks
import Loot.Base.HasLens (HasLens')
import Loot.Config ((:::), Config, PartialConfig)

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
type RocksDBParams =
   '[ "path"  ::: FilePath
      -- Path to the database
    , "clean" ::: Bool
      -- Whether DB should be cleaned/removed on start.
    ]

type RocksDBParamsRec = Config RocksDBParams
type RocksDBParamsRecP = PartialConfig RocksDBParams


data RocksDB = RocksDB
    { _rdDatabase :: !DB
    }

makeLenses ''RocksDB
