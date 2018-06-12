{-# LANGUAGE FunctionalDependencies #-}

-- | Common resources used by Disciplina nodes

module Disciplina.Launcher.Resource
       ( BasicNodeResources (..)
       , BracketResource(..)
       ) where

import Universum

import Control.Monad.Cont (ContT (..))
import System.Wlog (LoggerConfig (..), LoggerName, maybeLogsDirB, parseLoggerConfig, productionB,
                    removeAllHandlers, setupLogging, showTidB)

import Disciplina.DB.Real (DBParams, NodeDB, bracketNodeDB)
import Disciplina.Launcher.Params (BasicNodeParams (..), LoggingParams (..))

-- | Datatype which contains resources required by all Disciplina nodes
-- to start working.
data BasicNodeResources = BasicNodeResources
    { bnrLoggerName :: !LoggerName
    }

----------------------------------------------------------------------------
-- Resources
----------------------------------------------------------------------------

{- No top-level @acquire@s and @release@s for intension, having bracket built
from brackets of smaller resources automatially forces proper coupling of
acquire and release operations and so it's easier to use in various types of
node which require different resources.
-}

-- | Resources construction.
class BracketResource param res | param -> res, res -> param where
    -- | Construct a resource using given parameters. Automatic cleanup.
    bracketResource :: param -> (res -> IO a) -> IO a
    bracketResource = runContT . bracketResourceC

    {- | Version of 'bracketResource' convenient for building from
    smaller resources as soon as it gives do-notation.

    Compare:

    @
    bracketResource BasicNodeParams {..} = do
        bnrLoggerName <- bracketResource bnpLoggingParams
        bnrDB <- bracketResource bnpDBType bnpDBPath
        return BasicNodeResources {..}
    @

    with explicit variant

    @
    bracketResource BasicNodeParams {..} action =
        bracketResource bnpLoggingParams $ \bnrLoggerName ->
        bracketResource bnpDBType bnpDBPath $ \bnrDB ->
        action BasicNodeResources {..}
    @

    Make sure you do not drop continuation anywhere! Nothing will start then.
    (this won't happen if you do not do any Cont stuff by hand).
    -}
    bracketResourceC :: param -> ContT r IO res
    bracketResourceC = ContT . bracketResource

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

instance BracketResource LoggingParams LoggerName where
    bracketResource params = bracket pre fin
      where
        pre = setupLoggers params $> lpDefaultName params
        fin _ = removeAllHandlers

----------------------------------------------------------------------------
-- Bracket
----------------------------------------------------------------------------

instance BracketResource DBParams NodeDB where
    bracketResource = bracketNodeDB

instance BracketResource BasicNodeParams BasicNodeResources where
    bracketResourceC BasicNodeParams{..} = do
        bnrLoggerName <- bracketResourceC bnpLoggingParams
        return BasicNodeResources {..}
