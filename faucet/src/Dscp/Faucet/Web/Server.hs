{-# LANGUAGE TypeOperators #-}

-- | Functions to serve faucet HTTP API

module Dscp.Faucet.Web.Server
       ( serveFaucetAPIReal
       ) where

import Data.Reflection (reify)
import Fmt ((+|), (|+))
import Loot.Log (logInfo)
import Servant ((:>), Handler, Server, hoistServer, serve)
import Servant.Generic (toServant)

import Dscp.Faucet.Launcher (FaucetRealMode, FaucetWorkMode)
import Dscp.Faucet.Web.API (FaucetAPI, faucetAPI)
import Dscp.Faucet.Web.Handlers (convertFaucetApiHandler, faucetApiHandlers)
import Dscp.Util.Servant (LoggingApi)
import Dscp.Web (ServerParams (..), serveWeb)
import Dscp.Web.Server (buildServantLogConfig)

type FaucetWebAPI = "api" :> "faucet" :> FaucetAPI

mkFaucetApiServer
    :: forall ctx m. FaucetWorkMode ctx m
    => (forall x. m x -> Handler x)
    -> Server FaucetAPI
mkFaucetApiServer nat =
    hoistServer faucetAPI nat (toServant faucetApiHandlers)

serveFaucetAPIReal :: ServerParams -> FaucetRealMode ()
serveFaucetAPIReal ServerParams{..} = do
    logInfo $ "Serving faucet API on "+|spAddr|+""
    ctx <- ask
    lc <- buildServantLogConfig (<> "web")
    let faucetApiServer = mkFaucetApiServer (convertFaucetApiHandler ctx)
    serveWeb spAddr $
        reify lc $ \(_ :: Proxy lc) ->
        serve (Proxy @(LoggingApi lc FaucetWebAPI)) faucetApiServer
