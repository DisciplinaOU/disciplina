module Dscp.DB.SQLite.Types
       ( SQLiteDBLocation (..)
       , SQLiteDB (..)
       , SQLiteParams (..)
       ) where

import Database.SQLite.Simple (Connection)

-- | Where database lies.
data SQLiteDBLocation
    = SQLiteReal !FilePath  -- ^ In given file
    | SQLiteInMemory        -- ^ In memory

data SQLiteParams = SQLiteParams
    { sdpLocation :: SQLiteDBLocation
    }

newtype SQLiteDB = SQLiteDB { sdConn :: Connection }
