{-# LANGUAGE TypeOperators #-}

-- | Logging resource.

module Dscp.Resource.Logging
    ( LoggingParams(..)
    , basicLoggingParams
    , allocLogging
    , noLogging
    ) where

import Colog.Syslog (Collector (..), Facility (User), SyslogConfig (..))
import Control.Monad.Component (ComponentM)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (Object), (.:), (.=),
                   encode, object, withObject)
import Fmt ((+|), (|+))
import Loot.Log (BackendConfig (..), LogConfig (..), Logging (..), Name,
                 NameSelector (..), Severity (Debug, Info), allocateLogging,
                 fromLogAction, logDebug, modifyLogName)
import Loot.Log.Rio (LoggingIO)

import Dscp.Rio (runRIO)

----------------------------------------------------------------------------
-- Params
----------------------------------------------------------------------------

-- | Contains all parameters required for hierarchical logger initialization.
data LoggingParams = LoggingParams
    { lpConfig      :: !LogConfig
    -- ^ Logger configuration that will be used
    , lpDefaultName :: !Name
    -- ^ Logger name which will be used by default
    } deriving (Show, Eq)

-- | Creates a basic 'LoggingParams'
basicLoggingParams :: Name -> Bool -> LoggingParams
basicLoggingParams lpDefaultName debug = LoggingParams {..}
  where
    syslogConfig = SyslogConfig AutoLocal User (show lpDefaultName)
    backends = [StdErr, Syslog syslogConfig]
    minSeverity = if debug then Debug else Info
    lpConfig = LogConfig {..}

----------------------------------------------------------------------------
-- Other
----------------------------------------------------------------------------

allocLogging :: LoggingParams -> ComponentM LoggingIO
allocLogging LoggingParams{..} = do
    logging <- allocateLogging lpConfig (GivenName lpDefaultName)
    liftIO $ runRIO logging $ modifyLogName (<> "init" <> "log") $ do
        let configText = decodeUtf8 (encode lpConfig) :: Text
        logDebug $ "Logging config: "+|configText|+""
    return logging

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

instance FromJSON LoggingParams where
    parseJSON = withObject "LoggingParams" $ \v -> LoggingParams
        <$> parseJSON (Object v)
        -- short-circuiting: avoids adding a "config" key and parses a LogConfig directly
        <*> v .: "default-name"

instance ToJSON LoggingParams where
    toJSON LoggingParams {..} = object
        [ "default-name" .= lpDefaultName
        -- short-circuiting: the next 2 pairs actually belong to the 'LogConfig'
        , "backends"     .= backends lpConfig
        , "min-severity" .= minSeverity lpConfig
        ]
---------------------------------------------------------------------------
-- Misc
---------------------------------------------------------------------------

noLogging :: Monad m => Logging m
noLogging = fromLogAction CallstackName mempty
