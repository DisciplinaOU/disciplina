module Dscp.DB.SQLite.Types
       ( -- * SQLite bindings
         ConnectionString (..)
       , _ConnectionString
       , connStringEx
       , connStringFromText
       , PostgresRealParams (..)
       , prpConnStringL
       , prpConnNumL
       , prpMaxPendingL
       , PostgresDBMode (..)
       , _PostgresReal
       , SQL (..)
       , PostgresParams (..)
       , ppModeL
       ) where

import Control.Concurrent.Chan (Chan)
import Control.Lens (makeLensesWith, makePrisms)
import Data.Aeson (FromJSON (..))
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveFromJSON)
import Database.PostgreSQL.Simple (Connection)

import Dscp.Util

----------------------------------------------------------
-- Postgres bindings
----------------------------------------------------------

-- | Lib-pg connection string.
-- Normally looks like "".
newtype ConnectionString = ConnectionString ByteString
    deriving (Show, Eq, IsString)

makePrisms ''ConnectionString

-- | Example of 'ConnectionString'.
connStringEx :: ConnectionString
connStringEx = "postgresql:///my-db?host=localhost&port=5432"

-- | Make a connection string from textual representation.
connStringFromText :: ConvertUtf8 text ByteString => text -> ConnectionString
connStringFromText = ConnectionString . encodeUtf8

data PostgresRealParams = PostgresRealParams
    { prpConnString :: !ConnectionString
      -- ^ Path to the file with database.
    , prpConnNum    :: !(Maybe Int)
      -- ^ Connections pool size.
    , prpMaxPending :: !Int
      -- ^ Maximal number of requests waiting for a free connection.
    } deriving (Show, Eq)

makeLensesWith postfixLFields ''PostgresRealParams

-- | Database mode.
-- TODO: remove?
data PostgresDBMode
    = PostgresReal !PostgresRealParams
      -- ^ In given file using given number of connections.
    deriving (Show, Eq, Generic)

makePrisms ''PostgresDBMode

data PostgresParams = PostgresParams
    { ppMode :: PostgresDBMode
    } deriving (Show, Eq)

makeLensesWith postfixLFields ''PostgresParams

-- | Database context.
data SQL = SQL
    { sqlConnPool   :: Chan Connection
      -- ^ Connections to given database. Each connection is used no more than
      -- one thread at once - requirement of the database engine.
    , sqlConnNum    :: Int
      -- ^ Number of connections in pool
    , sqlPendingNum :: TVar Int
      -- ^ Number of threads waiting for free connection.
    , sqlMaxPending :: Int
      -- ^ Allowed number of pending threads.
    }

instance FromJSON PostgresDBMode
deriveFromJSON defaultOptions ''PostgresRealParams
deriveFromJSON defaultOptions ''PostgresParams

instance FromJSON ConnectionString where
    parseJSON v = connStringFromText <$> parseJSON @Text v
