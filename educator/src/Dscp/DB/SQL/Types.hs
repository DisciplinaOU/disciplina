{-# LANGUAGE OverloadedLabels #-}

module Dscp.DB.SQL.Types
       ( -- * SQL bindings
         ConnectionString (..)

       , PostgresRealParams
       , PostgresRealParamsRec
       , PostgresRealParamsRecP
       , PostgresTestParams (..)
       , PostgresDBMode (..)
       , TransactionsSwitch (..)
       , PostgresParams (..)
       , SQL (..)
       , PreparedSQL (..)

       , _ConnectionString
       , connStringFromText
       , _PostgresReal
       , defaultPostgresRealParams
       ) where

import Universum
import Control.Concurrent.Chan (Chan)
import Control.Lens (makePrisms, (?~))
import Data.Aeson (FromJSON (..))
import Database.PostgreSQL.Simple (Connection)
import GHC.TypeLits (Symbol)
import Loot.Config ((:::), Config, PartialConfig, option)

----------------------------------------------------------
-- Postgres bindings
----------------------------------------------------------

-- | Lib-pg connection string.
newtype ConnectionString = ConnectionString ByteString
    deriving (Show, Eq, IsString)

makePrisms ''ConnectionString

-- | Make a connection string from textual representation.
connStringFromText :: ConvertUtf8 text ByteString => text -> ConnectionString
connStringFromText = ConnectionString . encodeUtf8

type PostgresRealParams =
   '[ "connString" ::: ConnectionString
      -- Path to the file with database.
    , "connNum"    ::: Maybe Int
      -- Connections pool size.
    , "maxPending" ::: Int
      -- Maximal number of requests waiting for a free connection.
    ]

type PostgresRealParamsRec = Config PostgresRealParams
type PostgresRealParamsRecP = PartialConfig PostgresRealParams

defaultPostgresRealParams :: PostgresRealParamsRecP
defaultPostgresRealParams = mempty
    & option #connString ?~ "postgresql:///disciplina-educator"
    & option #connNum    ?~ Nothing
    & option #maxPending ?~ 200

data PostgresTestParams = PostgresTestParams
    { ptpConnString :: !ConnectionString
      -- ^ Connection string for a database.
    } deriving (Show, Eq)

-- | Database mode.
data PostgresDBMode
    = PostgresReal !PostgresRealParamsRec
      -- ^ Production settings.
    | PostgresTest !PostgresTestParams
      -- ^ Test settings.
    deriving (Show, Eq, Generic)

makePrisms ''PostgresDBMode

data PostgresParams = PostgresParams
    { ppMode :: PostgresDBMode
    } deriving (Show, Eq)


data TransactionsSwitch
    = RealTransactions
      -- ^ Transactions in code map to @BEGIN TRANSACTION@/@COMMIT@ statements.
    | TestTransactions
      -- [Note: sql-transactions-in-tests]
      -- ^ Transactions in code map to @SAVEPOINT@/@ROLLBACK TO + RELEASE SAVEPOINT@.

-- | Database context.
data SQL = SQL
    { sqlConnPool           :: Chan Connection
      -- ^ Connections to given database. Each connection is used no more than
      -- one thread at once - requirement of the database engine.
    , sqlConnNum            :: Int
      -- ^ Number of connections in pool
    , sqlPendingNum         :: TVar Int
      -- ^ Number of threads waiting for free connection.
    , sqlMaxPending         :: Int
      -- ^ Allowed number of pending threads.
    , sqlTransactionsSwitch :: TransactionsSwitch
      -- ^ How transactions are interpreted.
    }

-- | SQL with prepared schema described by type parameter @s@.
newtype PreparedSQL (s :: Symbol) = PreparedSQL { unPreparedSQL :: SQL }

instance FromJSON ConnectionString where
    parseJSON v = connStringFromText <$> parseJSON @Text v
