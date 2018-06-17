module Disciplina.Launcher.Params
       ( LoggingParams (..)
       ) where

import Universum

import System.Wlog (LoggerName)

-- | Contains all parameters required for hierarchical logger initialization.
data LoggingParams = LoggingParams
    { lpDefaultName :: !LoggerName
    -- ^ Logger name which will be used by default
    , lpDirectory   :: !(Maybe FilePath)
    -- ^ Path to log directory
    , lpConfigPath  :: !(Maybe FilePath)
    -- ^ Path to logger configuration
    } deriving Show
