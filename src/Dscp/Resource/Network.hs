-- | ZMQ TCP Networking resource allocation.

module Dscp.Resource.Network where

import Universum

import Control.Lens (makeLenses)
import Control.Monad.Component (buildComponent)
import Loot.Base.HasLens (HasLens (..), HasLens')
import Loot.Log (Level)
import Loot.Network.Class (NetworkingCli (..), NetworkingServ (..))
import Loot.Network.ZMQ (ZTGlobalEnv, ZTNetCliEnv, ZTNetServEnv, ZTNodeId (..), ZmqTcp,
                         createNetCliEnv, createNetServEnv, ztGlobalEnv, ztGlobalEnvRelease)
import qualified Loot.Network.ZMQ.Instance as Z

import Dscp.Launcher.Rio (RIO)
import Dscp.Resource.Class (AllocResource (..))

----------------------------------------------------------------------------
-- Common
----------------------------------------------------------------------------

-- TODO DSCP-105 Use log-warper here. Perhaps i should just put
-- logging function into parameters?
tempNetLog :: Level -> Text -> IO ()
tempNetLog l t = putTextLn $ "[" <> show l <> "]: " <> t

-- TODO Should I also release sockets?

----------------------------------------------------------------------------
-- Client
----------------------------------------------------------------------------

-- | Client networking params.
data NetCliParams = NetCliParams
    { ncPeers :: Set ZTNodeId
      -- ^ Peers we should connect to.
    } deriving (Eq,Show)

-- | Resources needed for ZMQ TCP client.
data NetCliResources = NetCliResources
    { _ncGlobalEnv :: ZTGlobalEnv
      -- ^ Global client environment.
    , _ncClientEnv :: ZTNetCliEnv
      -- ^ Client-specific environment.
    }

makeLenses ''NetCliResources

instance HasLens ZTGlobalEnv NetCliResources ZTGlobalEnv where lensOf = ncGlobalEnv
instance HasLens ZTNetCliEnv NetCliResources ZTNetCliEnv where lensOf = ncClientEnv

instance (HasLens' ctx ZTGlobalEnv, HasLens' ctx ZTNetCliEnv) =>
         NetworkingCli ZmqTcp (RIO ctx) where
    type NodeId ZmqTcp = Z.ZTNodeId
    runClient = Z.runClientDefault
    getPeers = Z.getPeersDefault
    updatePeers = Z.updatePeersDefault
    registerClient = Z.registerClientDefault

instance AllocResource NetCliParams NetCliResources where
    allocResource NetCliParams {ncPeers} =
        buildComponent "netcli" allocate release
      where
        allocate = do
            global <- ztGlobalEnv tempNetLog
            cli <- createNetCliEnv global ncPeers
            pure $ NetCliResources global cli
        release = ztGlobalEnvRelease . view ncGlobalEnv

----------------------------------------------------------------------------
-- Full node
----------------------------------------------------------------------------

-- | Server networking params.
data NetServParams = NetServParams
    { nsPeers      :: Set ZTNodeId
      -- ^ Peers we should connect to
    , nsOurAddress :: ZTNodeId
      -- ^ Our binding address
    } deriving (Eq,Show)

-- | Resources needed for ZMQ TCP client + server.
data NetServResources = NetServResources
    { _nsGlobalEnv :: ZTGlobalEnv
    , _nsClientEnv :: ZTNetCliEnv
    , _nsServerEnv :: ZTNetServEnv
    }

makeLenses ''NetServResources

instance HasLens ZTGlobalEnv NetServResources ZTGlobalEnv where lensOf = nsGlobalEnv
instance HasLens ZTNetCliEnv NetServResources ZTNetCliEnv where lensOf = nsClientEnv
instance HasLens ZTNetServEnv NetServResources ZTNetServEnv where lensOf = nsServerEnv

instance (HasLens' ctx ZTGlobalEnv, HasLens' ctx ZTNetServEnv) =>
         NetworkingServ ZmqTcp (RIO ctx) where
    type CliId ZmqTcp = Z.ZTCliId
    runServer = Z.runServerDefault
    registerListener = Z.registerListenerDefault

instance AllocResource NetServParams NetServResources where
    allocResource NetServParams {..} =
        buildComponent "netcli" allocate release
      where
        allocate = do
            global <- ztGlobalEnv tempNetLog
            cli <- createNetCliEnv global nsPeers
            serv <- createNetServEnv global nsOurAddress
            pure $ NetServResources global cli serv
        release = ztGlobalEnvRelease . view nsGlobalEnv
