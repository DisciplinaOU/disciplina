module Dscp.DB.SQLite.Types
       ( -- * SQLite bindings
         SQLiteRealParams (..)
       , SQLiteDBMode (..)
       , SQLiteDB (..)
       , SQLiteParams (..)

         -- * Educator schema
       , TxBlockIdx (..)
       , intTxBlockIdx
       ) where

import Control.Concurrent.Chan (Chan)
import Data.Aeson (FromJSON (..))
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveFromJSON)

import Control.Lens (Prism', prism)
import Database.SQLite.Simple (Connection)

----------------------------------------------------------
-- SQLite bindings
----------------------------------------------------------

data SQLiteRealParams = SQLiteRealParams
    { srpPath       :: !FilePath
      -- ^ Path to the file with database.
    , srpConnNum    :: !(Maybe Int)
      -- ^ Connections pool size.
    , srpMaxPending :: !Int
      -- ^ Maximal number of requests waiting for a free connection.
    }

-- | Database mode.
data SQLiteDBMode
    = SQLiteReal !SQLiteRealParams
      -- ^ In given file using given number of connections
    | SQLiteInMemory
      -- ^ In memory

data SQLiteParams = SQLiteParams
    { sdpMode :: SQLiteDBMode
    }

data SQLiteDB = SQLiteDB
    { sdConnPool   :: Chan Connection
      -- ^ Connections to given database. Each connection is used no more than
      -- one thread at once - requirement of SQLite.
    , sdConnNum    :: Int
      -- ^ Number of connections in pool
    , sdPendingNum :: TVar Int
      -- ^ Number of threads waiting for free connection.
    , sdMaxPending :: Int
      -- ^ Allowed number of pending threads.
    }

-- | Isomorphism between @Maybe FilePath@ and 'SQLiteDBMode'
maybeToSQLiteDBLoc :: Maybe FilePath -> SQLiteDBMode
maybeToSQLiteDBLoc Nothing     = SQLiteInMemory
maybeToSQLiteDBLoc (Just path) = SQLiteReal path

instance FromJSON SQLiteDBMode where
    parseJSON = fmap maybeToSQLiteDBLoc . parseJSON

deriveFromJSON defaultOptions ''SQLiteParams

----------------------------------------------------------
-- Educator schema
----------------------------------------------------------

-- | Schema internal: idx of transaction within block.
data TxBlockIdx
    = TxBlockIdx Word32
    | TxInMempool
    deriving (Eq, Show)

-- | Convert between 'TxBlockIdx' and true index which can be used in database.
intTxBlockIdx :: Prism' Int TxBlockIdx
intTxBlockIdx = prism toInt fromInt
  where
    toInt = \case
        TxBlockIdx idx -> fromIntegral idx
        TxInMempool    -> -1
    fromInt idx
        | idx >= 0 = Right $ TxBlockIdx (fromIntegral idx)
        | idx == -1 = Right $ TxInMempool
        | otherwise = Left idx
