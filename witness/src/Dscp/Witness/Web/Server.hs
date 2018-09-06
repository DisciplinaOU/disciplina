-- | Functions to serve wallet API.

module Dscp.Witness.Web.Server
       ( mkWitnessAPIServer
       , convertWitnessHandler
       , serveWitnessAPIReal
       ) where

import Data.Reflection (Reifies, reify)
import Fmt ((+|), (|+))
import Loot.Log (logInfo)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant (Handler, Server, hoistServer, serve, throwError)
import UnliftIO (UnliftIO (..), askUnliftIO)

import Dscp.Util.Servant (LoggingApi, ServantLogConfig (..))
import Dscp.Web (ServerParams (..), buildServantLogConfig, serveWeb)
import Dscp.Witness.Launcher.Mode (WitnessWorkMode)
import Dscp.Witness.Relay (RelayException)
import Dscp.Witness.Web.API (WitnessAPI, witnessAPI)
import Dscp.Witness.Web.Error
import Dscp.Witness.Web.Handlers (witnessServantHandlers)

mkWitnessAPIServer
    :: forall ctx m. WitnessWorkMode ctx m
    => (forall x. m x -> Handler x)
    -> Server WitnessAPI
mkWitnessAPIServer nat =
    hoistServer witnessAPI nat witnessServantHandlers

convertWitnessHandler
    :: UnliftIO m
    -> m a
    -> Handler a
convertWitnessHandler (UnliftIO unliftIO) handler =
    liftIO (unliftIO handler)
        `catch` throwServant
        `catch` (throwServant . ServiceUnavailable . show @Text @RelayException)
        `catchAny` (throwServant . InternalError . show)
  where
    throwServant = throwError . witnessToServantErr

serveWitnessAPIReal :: WitnessWorkMode ctx m => ServerParams -> m ()
serveWitnessAPIReal ServerParams{..} = do
    logInfo $ "Serving wallet API on "+|spAddr|+""
    unliftIO <- askUnliftIO
    lc <- buildServantLogConfig (<> "web")
    let ourCors = cors (const $ Just $
                        simpleCorsResourcePolicy
                        { corsRequestHeaders = [hContentType] })
    serveWeb spAddr $
        ourCors $
        reify lc $ \logConfigP ->
        serve (servedApi logConfigP) $
        mkWitnessAPIServer $
        convertWitnessHandler unliftIO
  where
    servedApi
        :: Reifies config ServantLogConfig
        => Proxy config -> Proxy (LoggingApi config WitnessAPI)
    servedApi _ = Proxy
