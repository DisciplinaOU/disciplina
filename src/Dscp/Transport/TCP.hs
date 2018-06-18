-- | Convenient TCP transport acquire and release with common configuration
-- options.

module Dscp.Transport.TCP
    ( bracketTransportTCP
    ) where

import Universum

import Data.Time.Units (Microsecond)
import Ether.Internal (HasLens (..))
import Fmt ((+||), (||+))
import Loot.Log (logError, modifyLogName)
import Loot.Log.Rio (LoggingIO)

import Network.QDisc.Fair (fairQDisc)
import qualified Network.Transport as NT (closeTransport)
import Network.Transport.Abstract (Transport)
import Network.Transport.Concrete (concrete)
import qualified Network.Transport.TCP as TCP

import Dscp.Launcher.Mode (RIO, runRIO)

bracketTransportTCP
    :: ( MonadIO n
       , HasLens LoggingIO ctx LoggingIO
       )
    => Microsecond
    -> TCP.TCPAddr
    -> (Transport n -> RIO ctx a)
    -> RIO ctx a
bracketTransportTCP connectionTimeout tcpAddr k = bracket
    (createTransportTCP connectionTimeout tcpAddr)
    snd
    (k . fst)

createTransportTCP
    :: forall n ctx.
       ( MonadIO n
       , HasLens LoggingIO ctx LoggingIO
       )
    => Microsecond -- ^ Connection timeout
    -> TCP.TCPAddr
    -> RIO ctx (Transport n, RIO ctx ())
createTransportTCP connectionTimeout addrInfo = do
    logging <- view $ lensOf @LoggingIO
    let tcpParams =
            (TCP.defaultTCPParameters
             { TCP.transportConnectTimeout =
                   Just $ fromIntegral connectionTimeout
             , TCP.tcpNewQDisc = fairQDisc $ \_ -> return Nothing
             -- Will check the peer's claimed host against the observed host
             -- when new connections are made. This prevents an easy denial
             -- of service attack.
             , TCP.tcpCheckPeerHost = True
             , TCP.tcpServerExceptionHandler = \e ->
                     runRIO logging $
                     modifyLogName (<> "transport") $
                         logError $ "Exception in tcp server: "+||e||+""
             })
    transportE <- liftIO $ TCP.createTransport addrInfo tcpParams
    case transportE of
        Left e -> do
            logError $ "Error creating TCP transport: "+||e||+""
            throwM e
        Right transport -> return (concrete transport, liftIO $ NT.closeTransport transport)
