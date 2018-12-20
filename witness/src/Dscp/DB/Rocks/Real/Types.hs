module Dscp.DB.Rocks.Real.Types
       ( MonadRealDB
       , DB (..)
       , RocksDBParams (..)
       , rdpPathL
       , rdpCleanL
       , RocksDB (..)
       , rdDatabase
       , Rocks.BatchOp (..)
       ) where

import Control.Lens (makeLenses, makeLensesWith)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveFromJSON)
import qualified Database.RocksDB as Rocks
import Loot.Base.HasLens (HasLens)

import Dscp.Util (postfixLFields)

-- | Set of constraints necessary to operate on real DB.
type MonadRealDB ctx m =
    ( MonadReader ctx m
    , HasLens ctx RocksDB
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
    { rdpPath  :: !FilePath
      -- ^ Path to the database
    , rdpClean :: !Bool
      -- ^ Whether DB should be cleaned/removed on start.
    } deriving (Show, Eq)

data RocksDB = RocksDB
    { _rdDatabase :: !DB
    }

makeLensesWith postfixLFields ''RocksDBParams
makeLenses ''RocksDB
deriveFromJSON defaultOptions ''RocksDBParams
