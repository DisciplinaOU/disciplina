{-# LANGUAGE FunctionalDependencies #-}

-- | Common resources used by Disciplina nodes

module Disciplina.Launcher.Resource
       ( AllocResource(..)
       ) where

import Universum

import Control.Monad.Component (ComponentM, buildComponent)
import System.Wlog (LoggerConfig (..), LoggerName, maybeLogsDirB, parseLoggerConfig, productionB,
                    removeAllHandlers, setupLogging, showTidB)

import Disciplina.DB.Real (DBParams, NodeDB, closeNodeDB, openNodeDB)
import Disciplina.Launcher.Params (LoggingParams (..))

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
readLoggerConfig = maybe (return productionB) parseLoggerConfig

getRealLoggerConfig :: MonadIO m => LoggingParams -> m LoggerConfig
getRealLoggerConfig LoggingParams{..} = do
    let cfgBuilder = productionB
                  <> showTidB
                  <> maybeLogsDirB lpDirectory
    cfg <- readLoggerConfig lpConfigPath
    pure $ cfg <> cfgBuilder

setupLoggers :: MonadIO m => LoggingParams -> m ()
setupLoggers params = setupLogging Nothing =<< getRealLoggerConfig params

instance AllocResource LoggingParams LoggerName where
    allocResource params = buildComponent "logging" pre fin
      where
        pre = setupLoggers params $> lpDefaultName params
        fin _ = removeAllHandlers

----------------------------------------------------------------------------
-- Bracket
----------------------------------------------------------------------------

instance AllocResource DBParams NodeDB where
    allocResource p = buildComponent "RocksDB" (openNodeDB p) closeNodeDB
