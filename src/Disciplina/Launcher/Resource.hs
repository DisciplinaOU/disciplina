
-- | Common resources used by Disciplina nodes

module Disciplina.Launcher.Resource
       ( BasicNodeResources (..)
       , bracketBasicNodeResources
       ) where

import Universum

import System.Wlog (LoggerConfig (..), LoggerName, maybeLogsDirB, parseLoggerConfig, productionB,
                    removeAllHandlers, setupLogging, showTidB)

import Disciplina.DB.Real (NodeDB, closeNodeDB, openNodeDB)
import Disciplina.Launcher.Params (BasicNodeParams (..), LoggingParams (..))

-- | Datatype which contains resources required by all Disciplina nodes
-- to start working.
data BasicNodeResources = BasicNodeResources
    { bnrLoggerName :: !LoggerName
    , bnrDB         :: !NodeDB
    }

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

----------------------------------------------------------------------------
-- Acquire/release/bracket
----------------------------------------------------------------------------

acquireBasicNodeResources ::
       BasicNodeParams
    -> IO BasicNodeResources
acquireBasicNodeResources BasicNodeParams {..} = do
    setupLoggers bnpLoggingParams
    let bnrLoggerName = lpDefaultName bnpLoggingParams
    bnrDB <- openNodeDB bnpDBType bnpDBPath
    return BasicNodeResources {..}

releaseBasicNodeResources ::
       BasicNodeResources
    -> IO ()
releaseBasicNodeResources BasicNodeResources {..} = do
    closeNodeDB bnrDB
    removeAllHandlers

bracketBasicNodeResources ::
       BasicNodeParams
    -> (BasicNodeResources -> IO a)
    -> IO a
bracketBasicNodeResources np =
    bracket (acquireBasicNodeResources np) releaseBasicNodeResources
