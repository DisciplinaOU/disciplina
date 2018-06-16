-- | Logging resource.
module Dscp.Resource.Logging
    ( LoggingParams(..)
    ) where

import Universum

import Control.Monad.Component (buildComponent)
import Data.Aeson (encode)
import Fmt ((+|), (|+))
import Loot.Log (Name, NameSelector (GivenName), logDebug, modifyLogName)
import Loot.Log.Rio (LoggingIO)
import Loot.Log.Warper (LoggerConfig, prepareLogWarper)
import System.Wlog (LoggerName, maybeLogsDirB, parseLoggerConfig, productionB, removeAllHandlers,
                    showTidB)

import Dscp.Launcher.Rio (runRIO)
import Dscp.Resource.Class (AllocResource (..))

----------------------------------------------------------------------------
-- Params
----------------------------------------------------------------------------

-- | Contains all parameters required for hierarchical logger initialization.
data LoggingParams = LoggingParams
    { lpDefaultName :: !Name
    -- ^ Logger name which will be used by default
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
