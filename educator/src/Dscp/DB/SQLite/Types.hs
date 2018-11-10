module Dscp.DB.SQLite.Types
       ( -- * SQLite bindings
         SQLiteRealParams (..)
       , srpPathL
       , srpConnNumL
       , srpMaxPendingL
       , SQLiteDBMode (..)
       , _SQLiteReal
       , _SQLiteInMemory
       , SQLiteDB (..)
       , SQLiteParams (..)
       , sdpModeL

         -- * Educator schema
       , TxBlockIdx (..)
       , txBlockIdxToInt
       , txBlockIdxFromInt
       ) where

import Control.Concurrent.Chan (Chan)
import Control.Lens (makeLensesWith, makePrisms)
import Data.Aeson (FromJSON (..))
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveFromJSON)
import Database.SQLite.Simple (Connection)

import Dscp.Util

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
    } deriving (Show, Eq)

makeLensesWith postfixLFields ''SQLiteRealParams

-- | Database mode.
data SQLiteDBMode
    = SQLiteReal !SQLiteRealParams
      -- ^ In given file using given number of connections
    | SQLiteInMemory
      -- ^ In memory
    deriving (Show, Eq)

makePrisms ''SQLiteDBMode

data SQLiteParams = SQLiteParams
    { sdpMode :: SQLiteDBMode
    } deriving (Show, Eq)

makeLensesWith postfixLFields ''SQLiteParams

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

deriveFromJSON defaultOptions ''SQLiteRealParams

-- | Isomorphism between @Maybe SQLiteRealParams@ and 'SQLiteDBMode'
maybeToSQLiteDBLoc :: Maybe SQLiteRealParams -> SQLiteDBMode
maybeToSQLiteDBLoc Nothing       = SQLiteInMemory
maybeToSQLiteDBLoc (Just params) = SQLiteReal params

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

txBlockIdxToInt :: TxBlockIdx -> Int
txBlockIdxToInt = \case
    TxBlockIdx idx -> fromIntegral idx
    TxInMempool    -> -1

txBlockIdxFromInt :: Int -> Either Text TxBlockIdx
txBlockIdxFromInt idx
    | idx >= 0 = Right $ TxBlockIdx (fromIntegral idx)
    | idx == -1 = Right $ TxInMempool
    | otherwise = Left $ "Bad transaction index within block: " <> pretty idx
