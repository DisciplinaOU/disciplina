{-# LANGUAGE OverloadedLabels #-}

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
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant (Handler, Server, StdMethod (..), hoistServer, serve, throwError)
import Servant.Util (LoggingApi, ServantLogConfig (..), methodsCoveringAPI)
import UnliftIO (UnliftIO (..), askUnliftIO)

import Dscp.Config (option)
import Dscp.Web (ServerParamsRec, buildServantLogConfig, serveWeb)
import Dscp.Web.Class
import Dscp.Witness.Launcher.Context
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
        `catchAny` (throwServant . InternalError . show)
  where
    throwServant = throwError . toServantErr @WitnessAPIError

-- | CORS is enabled to ease development for frontend team.
witnessCors :: Middleware
witnessCors = cors $ const $ Just $
    simpleCorsResourcePolicy
    { -- We use @Access-Control-Allow-Origin: *@ as soon as API is public.
      corsOrigins = Nothing
    , corsMethods = methodsCoveringAPI @['GET, 'POST, 'PUT] @WitnessAPI
    , corsRequestHeaders = [hContentType]
    }

serveWitnessAPIReal :: WitnessWorkMode ctx m => ServerParamsRec -> m a
serveWitnessAPIReal serverParams = do
    let spAddr = serverParams ^. option #addr
    logInfo $ "Serving wallet API on "+|spAddr|+""
    unliftIO <- askUnliftIO
    lc <- buildServantLogConfig (<> "web")
    serveWeb spAddr $
        witnessCors $
        reify lc $ \logConfigP ->
        serve (servedApi logConfigP) $
        mkWitnessAPIServer $
        convertWitnessHandler unliftIO
  where
    servedApi
        :: Reifies config ServantLogConfig
        => Proxy config -> Proxy (LoggingApi config WitnessAPI)
    servedApi _ = Proxy
