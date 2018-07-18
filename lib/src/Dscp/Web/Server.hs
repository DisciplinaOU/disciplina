
-- | Utils for serving HTTP APIs

module Dscp.Web.Server
       ( serveWeb
       ) where

import qualified Network.Wai.Handler.Warp as Warp
import Servant (Application)

import Dscp.Web.Types (NetworkAddress (..))

warpSettings :: NetworkAddress -> Warp.Settings
warpSettings NetworkAddress {..} = Warp.defaultSettings
    & Warp.setHost (fromString $ toString naHost)
    & Warp.setPort (fromIntegral naPort)

serveWeb
    :: MonadIO m
    => NetworkAddress -> Application -> m ()
serveWeb addr = liftIO . Warp.runSettings (warpSettings addr)
