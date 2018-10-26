{-# LANGUAGE TypeOperators #-}

-- | Logging resource.

module Dscp.Resource.Logging
    ( LoggingParams(..)
    , LoggingConfig (..)
    , basicLoggingParams
    , allocLogging
    , noLogging
    ) where

import Control.Applicative ((<|>))
import Control.Monad.Component (ComponentM, buildComponent)
import Data.Aeson (FromJSON (..), ToJSON, encode, withObject, (.:), (.:?), toJSON)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveFromJSON)
import Fmt ((+|), (|+))
import Loot.Log (Logging (..), Name, NameSelector (CallstackName, GivenName),
                 logInfo, modifyLogName, Level(Debug))
import Loot.Log.Rio (LoggingIO)
import Loot.Log.Syslog (SyslogConfig(..), loggerName, withPERROR, minLevel,
    stopLoggers, defaultLoggerConfig, prepareSyslog)
import Loot.Log.Warper (LoggerConfig, prepareLogWarper)
import System.Wlog (productionB, removeAllHandlers, showTidB)
import qualified Text.Show

import Dscp.Rio (runRIO)

----------------------------------------------------------------------------
-- Params
----------------------------------------------------------------------------

-- | Contains all parameters required for hierarchical logger initialization.
data LoggingParams = LoggingParams
    { lpConfig      :: !LoggingConfig
    -- ^ Logger configuration that will be used
    , lpDefaultName :: !Name
    -- ^ Logger name which will be used by default
    } deriving (Show, Eq)

-- Orphan instances required by tests, based on ToJSON for simplicity
instance Show LoggerConfig where
    show = decodeUtf8 . encode

instance Eq LoggerConfig where
    a == b = toJSON a == toJSON b

instance Eq SyslogConfig where
    a == b = toJSON a == toJSON b

-- | Contains the configuration for a logging type
data LoggingConfig
    = Warper LoggerConfig
    | Syslog SyslogConfig
    deriving (Show, Eq)

-- | Creates a basic 'LoggingParams': a slightly modified syslog default
basicLoggingParams :: Name -> Bool -> LoggingParams
basicLoggingParams lpDefaultName debug = LoggingParams {..}
  where
    lpConfig = Syslog $ SyslogConfig [simpleLoggerConfig]
    simpleLoggerConfig = defaultLoggerConfig
        & loggerName .~ show lpDefaultName
        & withPERROR .~ True
        & minLevel %~ if debug then const Debug else id

----------------------------------------------------------------------------
-- Other
----------------------------------------------------------------------------

allocLogging :: LoggingParams -> ComponentM LoggingIO
allocLogging LoggingParams{..} = buildComponent "logging" pre fin
  where
    givenName = GivenName lpDefaultName
    pre = case lpConfig of
        Warper config -> do
            (config', logging) <-
                prepareLogWarper (config <> productionB <> showTidB) givenName
            printCfg logging config'
            return logging
        Syslog config -> do
            logging <- prepareSyslog config givenName
            printCfg logging config
            return logging

    fin _ = case lpConfig of
        Warper _ -> removeAllHandlers
        Syslog _ -> stopLoggers

    printCfg :: (ToJSON c) => LoggingIO -> c -> IO ()
    printCfg logging finalConfig =
        runRIO logging $ modifyLogName (<> "init" <> "log") $ do
            let configText = decodeUtf8 (encode finalConfig) :: Text
            logInfo $ "Logging config: "+|configText|+""

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

instance FromJSON Name where
    parseJSON = fmap fromString . parseJSON

-- | 'LoggingConfig' has a quite flexible 'FromJSON' implementation:
-- It accepts 3 subsection: a "use" selector, "syslog" and "warper" configurations
-- If only one of the configs is defined it will be used, "use" is not necessary
-- If both are defined and "use" is not specified, "syslog" will be used
-- If "use" has a value of "syslog" or "warper", this chosen config will be used
instance FromJSON LoggingConfig where
    parseJSON = withObject "LoggingConfig" $ \v -> do
        let readConf val = case val :: Text of
                "syslog" -> Syslog <$> v .: "syslog"
                "warper" -> Warper <$> v .: "warper"
                name -> fail . toString $ "unknown logging selection: " <> name
        maybe (readConf "syslog" <|> readConf "warper") readConf =<< v .:? "use"

deriveFromJSON defaultOptions ''LoggingParams

---------------------------------------------------------------------------
-- Misc
---------------------------------------------------------------------------

noLogging :: Monad m => Logging m
noLogging = Logging
    { _log = \_ _ _ -> pass
    , _logName = pure CallstackName
    }
