-- | Functions to serve wallet API.

module Dscp.Witness.Web.Server
       ( serveWitnessAPIReal
       ) where

import Fmt ((+|), (|+))
import Loot.Log (logInfo)
import Servant (Handler, Server, hoistServer, serve, throwError)

import Dscp.Launcher.Rio (runRIO)
import Dscp.Web (ServerParams (..), serveWeb)
import Dscp.Witness.Config (HasWitnessConfig)
import Dscp.Witness.Launcher (WitnessContext, WitnessRealMode, WitnessWorkMode)
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
    serveWeb spAddr $
        serve witnessAPI $
        witnessAPIServer $
        convertWitnessHandler wCtx
