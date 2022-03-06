{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

-- | Logging resource.

module Dscp.Resource.Logging
    ( LoggingParams
    , LoggingParamsRec
    , LoggingParamsRecP
    , basicLoggingParams
    , allocLogging
    , noLogging
    ) where

import Universum
import Colog.Syslog (Collector (..), Facility (User), SyslogConfig (..))
import Control.Monad.Component (ComponentM)
import Data.Aeson (encode)
import Fmt ((+|), (|+))
import Loot.Config ((:::), (?~), Config, PartialConfig, option)
import Loot.Log (BackendConfig (..), LogConfig (..), Logging (..), Name,
                 NameSelector (..), Severity (Debug, Info), allocateLogging,
                 fromLogAction, logDebug, modifyLogName)
import Loot.Log.Rio (LoggingIO)

import Dscp.Rio (runRIO)

----------------------------------------------------------------------------
-- Params
----------------------------------------------------------------------------

-- | Contains all parameters required for hierarchical logger initialization.
type LoggingParams =
    '[ "config"      ::: LogConfig
       -- Logger configuration that will be used
     , "defaultName" ::: Name
       -- Logger name which will be used by default
     ]

type LoggingParamsRec = Config LoggingParams
type LoggingParamsRecP = PartialConfig LoggingParams

-- | Creates a basic 'LoggingParams' (Partial)
basicLoggingParams :: Name -> Bool -> LoggingParamsRecP
basicLoggingParams defaultName debug = mempty
    & option #config      ?~ LogConfig {..}
    & option #defaultName ?~ defaultName
  where
    syslogConfig = SyslogConfig AutoLocal User (show defaultName)
    backends     = [StdErr, Syslog syslogConfig]
    minSeverity  = if debug then Debug else Info

----------------------------------------------------------------------------
-- Other
----------------------------------------------------------------------------

allocLogging :: LoggingParamsRec -> ComponentM LoggingIO
allocLogging loggingParams = do
    let config      = loggingParams ^. option #config
        defaultName = loggingParams ^. option #defaultName
    logging <- allocateLogging config (GivenName defaultName)
    liftIO $ runRIO logging $ modifyLogName (<> "init" <> "log") $ do
        let configText = decodeUtf8 (encode config) :: Text
        logDebug $ "Logging config: "+|configText|+""
    return logging

---------------------------------------------------------------------------
-- Misc
---------------------------------------------------------------------------

noLogging :: Monad m => Logging m
noLogging = fromLogAction CallstackName mempty
