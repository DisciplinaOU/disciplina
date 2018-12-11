-- | Utils for serving HTTP APIs

module Dscp.Web.Server
       ( BaseUrl (..)
       , Scheme (..)
       , serveWeb
       , buildServantLogConfig
       , buildClientEnv
       ) where

import Control.Lens (views)
import Loot.Base.HasLens (HasLens, lensOf)
import Loot.Log (Logging, Message (..), MonadLogging, Name, NameSelector (..))
import qualified Loot.Log.Internal as Log
import Network.HTTP.Client.TLS (newTlsManager)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (Application)
import Servant.Client (BaseUrl (..), ClientEnv, Scheme (..), mkClientEnv)

import Dscp.Util.Servant (ServantLogConfig (..))
import Dscp.Web.Types (NetworkAddress (..))

warpSettings :: NetworkAddress -> Warp.Settings
warpSettings NetworkAddress {..} = Warp.defaultSettings
    & Warp.setHost (fromString $ toString naHost)
    & Warp.setPort (fromIntegral naPort)

----------------------------------------------------------------------------
-- Server stuff
----------------------------------------------------------------------------

serveWeb
    :: MonadIO m
    => NetworkAddress -> Application -> m ()
serveWeb addr = liftIO . Warp.runSettings (warpSettings addr)

-- | Grab logging context and build config for servant requests logging.
buildServantLogConfig
    :: (MonadReader ctx m, HasLens ctx (Logging IO), MonadLogging m)
    => (Name -> Name) -> m ServantLogConfig
buildServantLogConfig modifyName = do
    origNameSel <- Log.logName
    logger <- views lensOf Log._log
    let origName = case origNameSel of
                       GivenName x -> x
                       _           -> mempty
        name = modifyName origName
    return ServantLogConfig
        { clcLog = \severity text -> logger $ Message severity name text
        }

----------------------------------------------------------------------------
-- Client stuff
----------------------------------------------------------------------------

-- | Construct common client networking environment.
buildClientEnv :: MonadIO m => BaseUrl -> m ClientEnv
buildClientEnv baseUrl = do
    manager <- liftIO $ newTlsManager
    return $ mkClientEnv manager baseUrl
