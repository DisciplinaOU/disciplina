{-# LANGUAGE TypeOperators #-}

-- | Logging resource.

module Dscp.Resource.Logging
    ( LoggingParams(..)
    , LoggingType (..)
    , allocLogging
    , noLogging
    ) where

import Control.Monad.Component (ComponentM, buildComponent)
import Data.Aeson (FromJSON (..), ToJSON, encode, withText)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveFromJSON)
import Fmt ((+|), (|+))
import Loot.Log (Logging (..), Name, NameSelector (CallstackName, GivenName),
                 logInfo, logWarning, modifyLogName, Level(Debug))
import Loot.Log.Rio (LoggingIO)
import Loot.Log.Syslog (SyslogConfig(..), loggerName, withPERROR, minLevel,
    stopLoggers, syslogConfigFromFile, defaultLoggerConfig, prepareSyslog)
import Loot.Log.Warper (LoggerConfig, prepareLogWarper)
import System.Wlog (debugPlus, lcTree, ltSeverity, maybeLogsDirB, parseLoggerConfig, productionB,
                    removeAllHandlers, showTidB)

import Dscp.Rio (runRIO)

----------------------------------------------------------------------------
-- Params
----------------------------------------------------------------------------

-- | Contains all parameters required for hierarchical logger initialization.
data LoggingParams = LoggingParams
    { lpLoggingType :: !(Maybe LoggingType)
    -- ^ Logger system that will be use (Syslog if nothing is specified)
    , lpDefaultName :: !Name
    -- ^ Logger name which will be used by default
    , lpDebug       :: !Bool
    -- ^ When configuration file is not specified, this turns on
    -- console logging to debug.
    , lpDirectory   :: !(Maybe FilePath)
    -- ^ Path to log directory
    , lpConfigPath  :: !(Maybe FilePath)
    -- ^ Path to logger configuration
    } deriving (Show, Eq)

data LoggingType
    = Warper
    | Syslog
    deriving (Eq, Show, Read)

----------------------------------------------------------------------------
-- Other
----------------------------------------------------------------------------

getWarperLoggerConfig :: MonadIO m => LoggingParams -> m LoggerConfig
getWarperLoggerConfig LoggingParams{..} = do
    let tree = mempty & ltSeverity .~ Just debugPlus -- (bool infoPlus debugPlus lpDebug)
    let cfgBuilder =
            (productionB <>
             showTidB <>
             maybeLogsDirB lpDirectory)
            & lcTree .~ tree
    cfg <- readLoggerConfig lpConfigPath
    pure $ cfg <> cfgBuilder
  where
    readLoggerConfig :: MonadIO m => Maybe FilePath -> m LoggerConfig
    readLoggerConfig = maybe (pure productionB) parseLoggerConfig

getSyslogLoggerConfig :: MonadIO m => LoggingParams -> m (SyslogConfig, Maybe Text)
getSyslogLoggerConfig LoggingParams{..} =
    case lpConfigPath of
        Nothing -> return (fallbackLogConfig, Just "No configuration file specified")
        Just filePath -> syslogConfigFromFile filePath >>= \case
            Right config -> return (config, Nothing)
            Left e -> return (fallbackLogConfig, Just $ show e)
  where
    fallbackLogConfig = SyslogConfig [fallbackLoggerConfig]
    fallbackLoggerConfig = defaultLoggerConfig
        & loggerName .~ show lpDefaultName
        & withPERROR .~ True
        & minLevel %~ if lpDebug then const Debug else id

allocLogging :: LoggingParams -> ComponentM LoggingIO
allocLogging params = buildComponent "logging" pre fin
  where
    selectedType = fromMaybe Syslog $ lpLoggingType params -- Syslog is default

    pre = case selectedType of
        Warper -> do
            config <- getWarperLoggerConfig params
            (config', logging) <-
                prepareLogWarper config (GivenName $ lpDefaultName params)
            printCfg logging config' Nothing
            return logging
        Syslog -> do
            (config, warning) <- getSyslogLoggerConfig params
            logging <- prepareSyslog config (GivenName $ lpDefaultName params)
            printCfg logging config warning
            return logging

    fin _ = case selectedType of
        Warper -> removeAllHandlers
        Syslog -> stopLoggers

    printCfg :: (ToJSON c) => LoggingIO -> c -> Maybe Text-> IO ()
    printCfg logging finalConfig warning =
        runRIO logging $ modifyLogName (<> "init" <> "log") $ do
            let configText = decodeUtf8 (encode finalConfig) :: Text
            logInfo $ "Logging config: "+|configText|+""
            whenJust warning $ \message -> logWarning $
                "Problem with configuration loading: "+|message|+". Defaults \
                \will be used."

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

instance FromJSON Name where
    parseJSON = fmap fromString . parseJSON

instance FromJSON LoggingType where
    parseJSON = withText "LoggingType" $ \case
        "Syslog" -> pure Syslog
        "Warper" -> pure Warper
        _ -> fail "Unknown logging type. Available options: Syslog or Warper"

deriveFromJSON defaultOptions ''LoggingParams

---------------------------------------------------------------------------
-- Misc
---------------------------------------------------------------------------

noLogging :: Monad m => Logging m
noLogging = Logging
    { _log = \_ _ _ -> pass
    , _logName = pure CallstackName
    }
