-- | Logging resource.
module Dscp.Resource.Logging
    ( LoggingParams(..)
    , allocLogging
    ) where

import Control.Monad.Component (ComponentM, buildComponent)
import Data.Aeson (encode)
import Fmt ((+|), (|+))
import Loot.Log (Name, NameSelector (GivenName), logInfo, modifyLogName)
import Loot.Log.Rio (LoggingIO)
import Loot.Log.Warper (LoggerConfig, prepareLogWarper)
import System.Wlog (debugPlus, lcTree, ltSeverity, maybeLogsDirB, parseLoggerConfig, productionB,
                    removeAllHandlers, showTidB)

import Dscp.Launcher.Rio (runRIO)

----------------------------------------------------------------------------
-- Params
----------------------------------------------------------------------------

-- | Contains all parameters required for hierarchical logger initialization.
data LoggingParams = LoggingParams
    { lpDefaultName :: !Name
    -- ^ Logger name which will be used by default
    , lpDebug       :: !Bool
    -- ^ When configuration file is not specified, this turns on
    -- console logging to debug.
    , lpDirectory   :: !(Maybe FilePath)
    -- ^ Path to log directory
    , lpConfigPath  :: !(Maybe FilePath)
    -- ^ Path to logger configuration
    } deriving Show

----------------------------------------------------------------------------
-- Other
----------------------------------------------------------------------------

readLoggerConfig :: MonadIO m => Maybe FilePath -> m LoggerConfig
readLoggerConfig = maybe (pure productionB) parseLoggerConfig

getRealLoggerConfig :: MonadIO m => LoggingParams -> m LoggerConfig
getRealLoggerConfig LoggingParams{..} = do
    let tree = mempty & ltSeverity .~ Just debugPlus -- (bool infoPlus debugPlus lpDebug)
    let cfgBuilder =
            (productionB <>
             showTidB <>
             maybeLogsDirB lpDirectory)
            & lcTree .~ tree
    cfg <- readLoggerConfig lpConfigPath
    pure $ cfg <> cfgBuilder

allocLogging :: LoggingParams -> ComponentM LoggingIO
allocLogging params = buildComponent "logging" pre fin
  where
    pre = do
        config <- getRealLoggerConfig params
        (config', logging) <-
            prepareLogWarper config (GivenName $ lpDefaultName params)
        printCfg logging config
        printCfg logging config'
        return logging
    printCfg logging finalConfig =
        runRIO logging $ modifyLogName (<> "init" <> "log") $ do
            let configText = decodeUtf8 (encode finalConfig) :: Text
            logInfo $ "Logging config: "+|configText|+""
    fin _ = removeAllHandlers
