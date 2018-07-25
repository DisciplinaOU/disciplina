
-- | Utils for serving HTTP APIs

module Dscp.Web.Server
       ( serveWeb
       , buildServantLogConfig
       ) where

import Control.Lens (views)
import Loot.Base.HasLens (HasLens', lensOf)
import Loot.Log (Logging, MonadLogging, Name, _GivenName)
import qualified Loot.Log.Internal as Log
import qualified Network.Wai.Handler.Warp as Warp
import Servant (Application)

import Dscp.Util.Servant (ServantLogConfig (..))
import Dscp.Web.Types (NetworkAddress (..))

warpSettings :: NetworkAddress -> Warp.Settings
warpSettings NetworkAddress {..} = Warp.defaultSettings
    & Warp.setHost (fromString $ toString naHost)
    & Warp.setPort (fromIntegral naPort)

serveWeb
    :: MonadIO m
    => NetworkAddress -> Application -> m ()
serveWeb addr = liftIO . Warp.runSettings (warpSettings addr)

-- | Grab logging context and build config for servant requests logging.
buildServantLogConfig
    :: (MonadReader ctx m, HasLens' ctx (Logging IO), MonadLogging m)
    => (Name -> Name) -> m ServantLogConfig
buildServantLogConfig modifyName = do
    origNameSel <- Log.logName
    logger <- views (lensOf @(Logging IO)) Log._log
    let origName = fromMaybe mempty $ origNameSel ^? _GivenName
        name = modifyName origName
    return ServantLogConfig
        { clcLog = \level text -> logger level name text
        }
