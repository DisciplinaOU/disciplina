-- | Class for resource allocation.
--
-- Note that we split allocation into 2 phases:
-- 1. Logging is allocated separatelly as it needs for other resources
-- initialization.
-- 2. All other resources.

module Dscp.Resource.Class
       ( AllocResource(..)
       , InitParams (..)
       , InitContext (..)
       , buildComponentR
       ) where

import Universum
import Control.Lens (makeLenses)
import Control.Monad.Component (ComponentM, buildComponent)
import Loot.Base.HasLens (HasLens (..), HasLens')
import Loot.Log.Rio (LoggingIO)
import Time (sec)

import Dscp.Resource.Logging (LoggingParamsRec)
import Dscp.Rio (RIO, runRIO)
import Dscp.Util.TimeLimit

-- | Contains parameters required for most of resources allocation.
data InitParams = InitParams
    { ipLoggingParams :: LoggingParamsRec
    }

-- | Context used in most of resource allocations.
data InitContext = InitContext
    { _icLogging :: LoggingIO
    }

makeLenses ''InitContext

instance HasLens LoggingIO InitContext LoggingIO where
    lensOf = icLogging

-- | Resources safe allocation.
class AllocResource resource where
    -- | Data required for resource allocation.
    type Deps resource :: *

    -- | Construct a resource using given parameters. Automatic cleanup.
    -- Use 'buildComponentR' to construct function of this type.
    allocResource :: Deps resource -> ReaderT InitContext ComponentM resource

-- | 'buildComponent' for 'ReaderT'.
buildComponentR
    :: (HasLens' r LoggingIO)
    => Text
    -> RIO r a
    -> (a -> RIO r ())
    -> ReaderT r ComponentM a
buildComponentR desc allocate release =
    ReaderT $
      \r -> buildComponent desc
                (runRIO r allocate)
                (runRIO r . boundTeardown . release)
  where
    boundTeardown = boundExecution_ (sec 3) (desc <> " teardown")
