module Disciplina.DB.Real.Types
       ( DB (..)
       , DBType (..)
       , NodeDB (..)
       , ndbType
       , ndbDatabase
       ) where

import Universum

import Control.Lens (makeLenses)
import qualified Database.RocksDB as Rocks

data DB = DB
    { rocksReadOpts  :: !Rocks.ReadOptions
    , rocksWriteOpts :: !Rocks.WriteOptions
    , rocksOptions   :: !Rocks.Options
    , rocksDB        :: !Rocks.DB
    }

data DBType = WitnessDB
            | EducatorDB
            deriving Show

data NodeDB = NodeDB
    { _ndbType     :: !DBType
    , _ndbDatabase :: !DB
    }

makeLenses ''NodeDB
