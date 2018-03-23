module Disciplina.Launcher.Params
       ( BasicNodeParams (..)
       , LoggingParams (..)
       ) where

import Universum

import System.Wlog (LoggerName)

import Disciplina.DB.Types (DBType)

-- | Contains all parameters required for hierarchical logger initialization.
data LoggingParams = LoggingParams
    { lpDefaultName :: !LoggerName
    -- ^ Logger name which will be used by default
    , lpDirectory   :: !(Maybe FilePath)
    -- ^ Path to log directory
    , lpConfigPath  :: !(Maybe FilePath)
    -- ^ Path to logger configuration
    } deriving Show

-- | Contains all initialization parameters which all Disciplina nodes share.
data BasicNodeParams = BasicNodeParams
    { bnpLoggingParams :: !LoggingParams
    -- ^ Logging params
    , bnpDBType        :: !DBType
    -- ^ Database type (Witness or Node)
    , bnpDBPath        :: !FilePath
    -- ^ Path to the database
    } deriving Show
