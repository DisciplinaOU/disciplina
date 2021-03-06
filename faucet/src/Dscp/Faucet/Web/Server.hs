{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

-- | Functions to serve faucet HTTP API

module Dscp.Faucet.Web.Server
       ( serveFaucetAPIReal
       ) where

import Data.Reflection (reify)
import Fmt ((+|), (|+))
import Loot.Log (logInfo)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant (Handler, Server, StdMethod (..), hoistServer, serve)
import Servant.Generic (toServant)
import Servant.Util (LoggingApi, methodsCoveringAPI)

import Dscp.Config
import Dscp.Faucet.Config
import Dscp.Faucet.Launcher (FaucetRealMode, FaucetWorkMode)
import Dscp.Faucet.Web.API (FaucetAPI, faucetAPI)
import Dscp.Faucet.Web.Handlers (convertFaucetApiHandler, faucetApiHandlers)
import Dscp.Web (serveWeb)
import Dscp.Web.Server (buildServantLogConfig)

mkFaucetApiServer
    :: forall ctx m. FaucetWorkMode ctx m
    => (forall x. m x -> Handler x)
    -> Server FaucetAPI
mkFaucetApiServer nat =
    hoistServer faucetAPI nat (toServant faucetApiHandlers)

-- | CORS is enabled to ease development for frontend team.
faucetCors :: Middleware
faucetCors = cors $ const $ Just $
    simpleCorsResourcePolicy
    { -- We use @Access-Control-Allow-Origin: *@ as soon as API is public.
      corsOrigins = Nothing
    , corsMethods = methodsCoveringAPI @['GET, 'POST] @FaucetAPI
    , corsRequestHeaders = [hContentType]
    }

serveFaucetAPIReal :: HasFaucetConfig => FaucetRealMode a
serveFaucetAPIReal = do
    let spAddr = faucetConfig ^. sub #faucet . sub #api . option #addr
    logInfo $ "Serving faucet API on "+|spAddr|+""
    ctx <- ask
    lc <- buildServantLogConfig (<> "web")
    let faucetApiServer = mkFaucetApiServer (convertFaucetApiHandler ctx)
    serveWeb spAddr $
        faucetCors $
        reify lc $ \(_ :: Proxy lc) ->
            serve (Proxy @(LoggingApi lc FaucetAPI)) faucetApiServer
