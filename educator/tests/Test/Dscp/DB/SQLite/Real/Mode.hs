{-# LANGUAGE OverloadedLabels #-}

module Test.Dscp.DB.SQLite.Real.Mode
    ( runSQLiteMode
    , launchSQLiteMode
    ) where

import Control.Lens ((?~))
import System.IO.Temp (withSystemTempFile)

import Dscp.Config
import Dscp.DB.SQLite
import Dscp.Rio

-- | Test parameters for db which is stored in filesystem.
testRealSQLiteParams :: FilePath -> SQLiteParamsRec
testRealSQLiteParams dbPath = finaliseDeferredUnsafe $ mempty
    & tree #mode . selection ?~ "real"
    & tree #mode . branch #real . option #path       ?~ dbPath
    & tree #mode . branch #real . option #connNum    ?~ Just 5
    & tree #mode . branch #real . option #maxPending ?~ 1000

-- | Run an action with context supplied with database stored in the provided file.
runSQLiteMode :: FilePath -> RIO SQLiteDB a -> IO a
runSQLiteMode dbPath action = do
    bracket (openSQLiteDB dbParams)
            (\db -> closeSQLiteDB db) $
            \db -> runRIO db action
  where
    dbParams = testRealSQLiteParams dbPath

-- | Run an action with a database at temporary location.
launchSQLiteMode :: RIO SQLiteDB a -> IO a
launchSQLiteMode action =
    withSystemTempFile "test-db-XXX.sql" $ \tmpFile _hdl -> do
        runSQLiteMode tmpFile action
