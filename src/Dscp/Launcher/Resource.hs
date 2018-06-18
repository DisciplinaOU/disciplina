{-# LANGUAGE FunctionalDependencies #-}

-- | Common resources used by Disciplina nodes

module Dscp.Launcher.Resource
       ( AllocResource(..)
       ) where

import Universum

import Control.Monad.Component (ComponentM, buildComponent)
import Data.Aeson (encode)
import Fmt ((+|), (|+))
import Loot.Log (NameSelector (GivenName), logDebug, modifyLogName)
import Loot.Log.Rio (LoggingIO)
import Loot.Log.Warper (LoggerConfig, prepareLogWarper)
import System.Wlog (maybeLogsDirB, parseLoggerConfig, productionB, removeAllHandlers, showTidB)

import Dscp.DB.Rocks.Real (RocksDB, RocksDBParams, closeNodeDB, openNodeDB)
import Dscp.DB.SQLite (SQLiteDB, SQLiteParams, closeSQLiteDB, openSQLiteDB)
import Dscp.Launcher.Params (LoggingParams (..))
import Dscp.Launcher.Rio (runRIO)

----------------------------------------------------------------------------
-- Resources
----------------------------------------------------------------------------

-- | Resources safe allocation.
class AllocResource param resource | param -> resource, resource -> param where
    -- | Construct a resource using given parameters. Automatic cleanup.
    -- Use 'buildComponent' to construct function of this type.
    allocResource :: param -> ComponentM resource

----------------------------------------------------------------------------
-- Logging
----------------------------------------------------------------------------

readLoggerConfig :: MonadIO m => Maybe FilePath -> m LoggerConfig
readLoggerConfig = maybe (pure productionB) parseLoggerConfig

getRealLoggerConfig :: MonadIO m => LoggingParams -> m LoggerConfig
getRealLoggerConfig LoggingParams{..} = do
    let cfgBuilder = productionB
                  <> showTidB
                  <> maybeLogsDirB lpDirectory
    cfg <- readLoggerConfig lpConfigPath
    pure $ cfg <> cfgBuilder

instance AllocResource LoggingParams LoggingIO where
    allocResource params = buildComponent "logging" pre fin
      where
        pre = do
            config <- getRealLoggerConfig params
            (config', logging) <- prepareLogWarper config (GivenName mempty)
            printCfg logging config'
            return logging
        printCfg logging finalConfig =
            runRIO logging $ modifyLogName (<> "init" <> "log") $ do
                let configText = decodeUtf8 (encode finalConfig) :: Text
                logDebug $ "Logging config: "+|configText|+""
        fin _ = removeAllHandlers

----------------------------------------------------------------------------
-- Bracket
----------------------------------------------------------------------------

instance AllocResource RocksDBParams RocksDB where
    allocResource p = buildComponent "RocksDB" (openNodeDB p) closeNodeDB

instance AllocResource SQLiteParams SQLiteDB where
    allocResource p = buildComponent "SQLite DB" (openSQLiteDB p) closeSQLiteDB
