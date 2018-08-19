-- | Functions to serve wallet API.

module Dscp.Witness.Web.Server
       ( serveWitnessAPIReal
       ) where

import Data.Reflection (Reifies, reify)
import Fmt ((+|), (|+))
import Loot.Log (logInfo)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant (Handler, Server, hoistServer, serve, throwError)

import Dscp.Rio (runRIO)
import Dscp.Util.Servant (LoggingApi, ServantLogConfig (..))
import Dscp.Web (ServerParams (..), buildServantLogConfig, serveWeb)
import Dscp.Witness.Config (HasWitnessConfig)
import Dscp.Witness.Launcher.Mode (WitnessContext, WitnessRealMode, WitnessWorkMode)
import Dscp.Witness.Web.API (WitnessAPI, witnessAPI)
import Dscp.Witness.Web.Error
import Dscp.Witness.Web.Handlers (witnessServantHandlers)

witnessAPIServer
    :: forall ctx m. WitnessWorkMode ctx m
    => (forall x. m x -> Handler x)
    -> Server WitnessAPI
witnessAPIServer nat =
    hoistServer witnessAPI nat witnessServantHandlers

convertWitnessHandler
    :: WitnessContext
    -> WitnessRealMode a
    -> Handler a
convertWitnessHandler ctx handler =
    liftIO (runRIO ctx handler)
        `catch` (throwError . witnessToServantErr)
        `catchAny` (throwError . witnessToServantErr . InternalError . show)

serveWitnessAPIReal :: HasWitnessConfig => ServerParams -> WitnessRealMode ()
serveWitnessAPIReal ServerParams{..} = do
    logInfo $ "Serving wallet API on "+|spAddr|+""
    wCtx <- ask
    lc <- buildServantLogConfig (<> "web")
    let ourCors = cors (const $ Just $
                        simpleCorsResourcePolicy
                        { corsRequestHeaders = [hContentType] })
    serveWeb spAddr $
        ourCors $
        reify lc $ \logConfigP ->
        serve (servedApi logConfigP) $
        witnessAPIServer $
        convertWitnessHandler wCtx
  where
    servedApi
        :: Reifies config ServantLogConfig
        => Proxy config -> Proxy (LoggingApi config WitnessAPI)
    servedApi _ = Proxy
