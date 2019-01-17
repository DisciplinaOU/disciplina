{-# LANGUAGE OverloadedLabels #-}

module Dscp.DB.SQLite.Types
       ( -- * SQLite bindings
         SQLiteRealParams
       , SQLiteRealParamsRec
       , SQLiteRealParamsRecP

       , SQLiteParams
       , SQLiteParamsRec
       , SQLiteParamsRecP
       , defaultSQLiteParams

       , SQLiteDB (..)
       ) where

import Control.Concurrent.Chan (Chan)
import Database.SQLite.Simple (Connection)
import Loot.Config ((::+), (:::), (::-), Config, PartialConfig, tree, branch,
                    selection, option, (?~))

----------------------------------------------------------
-- SQLite bindings
----------------------------------------------------------

type SQLiteRealParams =
   '[ "path"       ::: FilePath
      -- Path to the file with database.
    , "connNum"    ::: Maybe Int
      -- Connections pool size.
    , "maxPending" ::: Int
      -- Maximal number of requests waiting for a free connection.
    ]

type SQLiteRealParamsRec = Config SQLiteRealParams
type SQLiteRealParamsRecP = PartialConfig SQLiteRealParams

-- | Database mode.
-- Note, there is a reason this contains the whole tree and not just its content
-- see 'ConfigMaybe' for an explanation.
type SQLiteParams =
   '[ "mode" ::+
       '[ "real"     ::- SQLiteRealParams
          -- In given file using given number of connections
        , "inMemory" ::- '[]
          -- In memory
        ]
    ]

type SQLiteParamsRec = Config SQLiteParams
type SQLiteParamsRecP = PartialConfig SQLiteParams

defaultSQLiteParams :: SQLiteParamsRecP
defaultSQLiteParams = mempty
    & tree #mode . selection ?~ "real"
    & tree #mode . branch #real . option #path       ?~ "educator-db"
    & tree #mode . branch #real . option #connNum    ?~ Nothing
    & tree #mode . branch #real . option #maxPending ?~ 200


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
