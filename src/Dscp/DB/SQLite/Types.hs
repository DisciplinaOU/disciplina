module Dscp.DB.SQLite.Types
       ( MonadSQLiteDB
       , SQLiteDBLocation (..)
       , SQLiteDB (..)
       , SQLiteParams (..)
       ) where

import Universum

import Database.SQLite.Simple (Connection)
import Ether.Internal (HasLens)

-- | Set of constraints necessary to operate on SQLite database.
type MonadSQLiteDB ctx m =
    ( MonadReader ctx m
    , HasLens SQLiteDB ctx SQLiteDB
    , MonadIO m
    , Monad m
    )

-- | Where database lies.
data SQLiteDBLocation
    = SQLiteReal !FilePath  -- ^ In given file
    | SQLiteInMemory        -- ^ In memory

data SQLiteParams = SQLiteParams
    { sdpLocation :: SQLiteDBLocation
    }

newtype SQLiteDB = SQLiteDB { sdConn :: Connection }
