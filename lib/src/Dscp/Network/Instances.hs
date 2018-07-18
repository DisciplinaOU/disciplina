-- | Instances for the ReaderT ctx IO pattern.

module Dscp.Network.Instances () where

import Loot.Base.HasLens (HasLens')
import Loot.Network.Class (NetworkingCli (..), NetworkingServ (..))
import Loot.Network.ZMQ (ZTGlobalEnv, ZTNetCliEnv, ZTNetServEnv, ZmqTcp)
import qualified Loot.Network.ZMQ.Instance as Z

import Dscp.Launcher.Rio (RIO)


instance (HasLens' ctx ZTGlobalEnv, HasLens' ctx ZTNetCliEnv) =>
         NetworkingCli ZmqTcp (RIO ctx) where
    type NodeId ZmqTcp = Z.ZTNodeId
    runClient = Z.runClientDefault
    getPeers = Z.getPeersDefault
    updatePeers = Z.updatePeersDefault
    registerClient = Z.registerClientDefault

instance (HasLens' ctx ZTGlobalEnv, HasLens' ctx ZTNetServEnv) =>
         NetworkingServ ZmqTcp (RIO ctx) where
    type CliId ZmqTcp = Z.ZTCliId
    runServer = Z.runServerDefault
    registerListener = Z.registerListenerDefault
