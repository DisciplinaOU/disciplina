module Dscp.Launcher.Params
       ( LoggingParams (..)
       ) where

import Universum

import qualified Loot.Log as Log

-- | Contains all parameters required for hierarchical logger initialization.
data LoggingParams = LoggingParams
    { lpDefaultName :: !Log.Name
    -- ^ Logger name which will be used by default
    , lpDirectory   :: !(Maybe FilePath)
    -- ^ Path to log directory
    , lpConfigPath  :: !(Maybe FilePath)
    -- ^ Path to logger configuration
    } deriving Show
