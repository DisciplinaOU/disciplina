-- | ZMQ TCP Networking resource allocation.

module Dscp.Resource.Network
    (
      NetLogging(..)
    , WithNetLogging
    , withNetLogging
    , netLogging

    , NetCliParams(..)
    , NetCliResources(..)

    , NetServParams(..)
    , NetServResources(..)
    ) where

import Control.Lens (makeLenses)
import Control.Monad.Component (buildComponent)
import Data.Reflection (Given (given), give)
import Loot.Base.HasLens (HasLens (..))
import Loot.Log (Logging)
import Loot.Network.ZMQ (ZTGlobalEnv, ZTNetCliEnv, ZTNetServEnv, ZTNodeId (..), createNetCliEnv,
                         createNetServEnv, ztGlobalEnv, ztGlobalEnvRelease)
import qualified Text.Show

import Dscp.Resource.Class (AllocResource (..))

----------------------------------------------------------------------------
-- Common
----------------------------------------------------------------------------

-- There are two ways to pass logging function into allocation context.
-- 1. Directly using params. Cons: params now have parser, but logging
--    function is of other kind. So either parsers should be rewritten
--    or they should accept function as a parameter. Both ways are terrible imo.
-- 2. Using reflection. Less terrible, but still terrible.
--
-- In fact, reflection is used only once -- when performing the allocation.
-- Logging is a resource as well, so it's initialised first and its
-- result is passed to networking allocator. In this way, the number
-- of "withNetLogging" usages is minimised to (for now) one place, which
-- is AllocResource instance where network resources are initialised.

-- | Newtype over logging function network threads will be using.
newtype NetLogging = NetLogging { unNetLogging :: Logging IO }

instance Show NetLogging where
    show _ = "NetLogging"

type WithNetLogging = Given NetLogging

withNetLogging :: NetLogging -> (WithNetLogging => r) -> r
withNetLogging = give

netLogging :: WithNetLogging => NetLogging
netLogging = given

----------------------------------------------------------------------------
-- Client
----------------------------------------------------------------------------

-- | Client networking params.
data NetCliParams = NetCliParams
    { ncPeers   :: !(Set ZTNodeId)
      -- ^ Peers we should connect to.
    } deriving (Show)

-- | Resources needed for ZMQ TCP client.
data NetCliResources = NetCliResources
    { _ncGlobalEnv :: !ZTGlobalEnv
      -- ^ Global client environment.
    , _ncClientEnv :: !ZTNetCliEnv
      -- ^ Client-specific environment.
    }

makeLenses ''NetCliResources

instance HasLens ZTGlobalEnv NetCliResources ZTGlobalEnv where lensOf = ncGlobalEnv
instance HasLens ZTNetCliEnv NetCliResources ZTNetCliEnv where lensOf = ncClientEnv

instance WithNetLogging => AllocResource NetCliParams NetCliResources where
    allocResource NetCliParams {..} =
        buildComponent "netcli" allocate release
      where
        allocate = do
            global <- ztGlobalEnv (unNetLogging netLogging)
            cli <- createNetCliEnv global ncPeers
            pure $ NetCliResources global cli
        release = ztGlobalEnvRelease . view ncGlobalEnv

----------------------------------------------------------------------------
-- Full node
----------------------------------------------------------------------------

-- | Server networking params.
data NetServParams = NetServParams
    { nsPeers      :: !(Set ZTNodeId)
      -- ^ Peers we should connect to
    , nsOurAddress :: !ZTNodeId
      -- ^ Our binding address
    } deriving (Show)

-- | Resources needed for ZMQ TCP client + server.
data NetServResources = NetServResources
    { _nsGlobalEnv :: !ZTGlobalEnv
    , _nsClientEnv :: !ZTNetCliEnv
    , _nsServerEnv :: !ZTNetServEnv
    }

makeLenses ''NetServResources

instance HasLens ZTGlobalEnv NetServResources ZTGlobalEnv where lensOf = nsGlobalEnv
instance HasLens ZTNetCliEnv NetServResources ZTNetCliEnv where lensOf = nsClientEnv
instance HasLens ZTNetServEnv NetServResources ZTNetServEnv where lensOf = nsServerEnv

instance WithNetLogging => AllocResource NetServParams NetServResources where
    allocResource NetServParams {..} =
        buildComponent "netcli" allocate release
      where
        allocate = do
            global <- ztGlobalEnv (unNetLogging netLogging)
            cli <- createNetCliEnv global nsPeers
            serv <- createNetServEnv global nsOurAddress
            pure $ NetServResources global cli serv
        release = ztGlobalEnvRelease . view nsGlobalEnv
