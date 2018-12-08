module Dscp.DB.SQLite.Types
       ( -- * SQLite bindings
         PostgresRealParams (..)
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

data PostgresRealParams = PostgresRealParams
    { prpConnString :: !Text
      -- ^ Path to the file with database.
    , prpConnNum    :: !(Maybe Int)
      -- ^ Connections pool size.
    , prpMaxPending :: !Int
      -- ^ Maximal number of requests waiting for a free connection.
    } deriving (Show, Eq)

makeLensesWith postfixLFields ''PostgresRealParams

-- | Database mode.
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

deriveFromJSON defaultOptions ''PostgresRealParams

instance FromJSON PostgresDBMode

deriveFromJSON defaultOptions ''PostgresParams
