module Dscp.DB.Rocks.Real.Types
       ( MonadRealDB
       , DB (..)
       , DBParams (..)
       , NodeDB (..)
       , ndbDatabase
       ) where

import Universum

import Control.Lens (makeLenses)
import qualified Database.RocksDB as Rocks
import Ether.Internal (HasLens)

-- | Set of constraints necessary to operate on real DB.
type MonadRealDB ctx m =
    ( MonadReader ctx m
    , HasLens NodeDB ctx NodeDB
    , MonadIO m
    , Monad m
    )

data DB = DB
    { rocksReadOpts  :: !Rocks.ReadOptions
    , rocksWriteOpts :: !Rocks.WriteOptions
    , rocksOptions   :: !Rocks.Options
    , rocksDB        :: !Rocks.DB
    }

-- | Set of parameters provided on opening connection.
data DBParams = DBParams
    { dbpPath :: !FilePath
    -- ^ Path to the database
    } deriving Show

data NodeDB = NodeDB
    { _ndbDatabase :: !DB
    }

makeLenses ''NodeDB
