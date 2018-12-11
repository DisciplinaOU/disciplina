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
import Data.Aeson (FromJSON (..), withText)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveFromJSON)
import Data.Default (def)
import Data.Reflection (Given (given), give)
import Loot.Base.HasLens (HasLens (..))
import Loot.Log (Logging)
import Loot.Network.ZMQ (ZTGlobalEnv, ZTNetCliEnv, ZTNetServEnv, ZTNodeId, createNetCliEnv,
                         createNetServEnv, parseZTNodeId, termNetCliEnv, termNetServEnv,
                         ztGlobalEnv, ztGlobalEnvRelease)
import qualified Text.Show

import Dscp.Resource.Class (AllocResource (..), buildComponentR)

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
    { ncPeers   :: ![ZTNodeId]
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

instance HasLens NetCliResources ZTGlobalEnv where lensOf = ncGlobalEnv
instance HasLens NetCliResources ZTNetCliEnv where lensOf = ncClientEnv

instance WithNetLogging => AllocResource NetCliResources where
    type Deps NetCliResources = NetCliParams
    allocResource NetCliParams {..} =
        buildComponentR "netcli" allocate release
      where
        allocate = liftIO $ do
            global <- ztGlobalEnv (unNetLogging netLogging)
            cli <- createNetCliEnv global def ncPeers
            pure $ NetCliResources global cli
        release NetCliResources{..} = do
            termNetCliEnv _ncClientEnv
            ztGlobalEnvRelease _ncGlobalEnv

----------------------------------------------------------------------------
-- Full node
----------------------------------------------------------------------------

-- | Server networking params.
data NetServParams = NetServParams
    { nsPeers      :: ![ZTNodeId]
      -- ^ Peers we should connect to
    , nsOurAddress :: !ZTNodeId
      -- ^ Our binding address
    } deriving (Show, Eq)

-- | Resources needed for ZMQ TCP client + server.
data NetServResources = NetServResources
    { _nsGlobalEnv :: !ZTGlobalEnv
    , _nsClientEnv :: !ZTNetCliEnv
    , _nsServerEnv :: !ZTNetServEnv
    }

makeLenses ''NetServResources

instance HasLens NetServResources ZTGlobalEnv where lensOf = nsGlobalEnv
instance HasLens NetServResources ZTNetCliEnv where lensOf = nsClientEnv
instance HasLens NetServResources ZTNetServEnv where lensOf = nsServerEnv

instance WithNetLogging => AllocResource NetServResources where
    type Deps NetServResources = NetServParams
    allocResource NetServParams {..} =
        buildComponentR "netcli" allocate release
      where
        allocate = liftIO $ do
            global <- ztGlobalEnv (unNetLogging netLogging)
            cli <- createNetCliEnv global def nsPeers
            serv <- createNetServEnv global def nsOurAddress

            pure $ NetServResources global cli serv
        release NetServResources{..} = liftIO $ do
            termNetCliEnv _nsClientEnv
            termNetServEnv _nsServerEnv
            ztGlobalEnvRelease _nsGlobalEnv

---------------------------------------------------------------------------
-- JSON instances for params
---------------------------------------------------------------------------

instance FromJSON ZTNodeId where
    parseJSON = withText "ZMQ node ID" $
        either fail pure . parseZTNodeId . toString

deriveFromJSON defaultOptions ''NetCliParams
deriveFromJSON defaultOptions ''NetServParams
