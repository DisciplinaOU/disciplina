-- | Functions to serve Student HTTP API

module Dscp.Educator.Web.Student.Server
       ( serveStudentAPIReal
       ) where

import Fmt ((+|), (|+))
import Loot.Log (logInfo)
import Servant (Handler, Server, hoistServer, serve, throwError)

import Dscp.Educator.Config (HasEducatorConfig)
import Dscp.Educator.Launcher (EducatorContext, EducatorRealMode, EducatorWorkMode)
import Dscp.Educator.Web.Student.API (StudentAPI, studentAPI)
import Dscp.Educator.Web.Student.Error (toServantErr, unexpectedToServantErr)
import Dscp.Educator.Web.Student.Handlers (servantHandlers)
import Dscp.Launcher.Rio (runRIO)
import Dscp.Web (NetworkAddress, serveWeb)

studentAPIServer
    :: forall ctx m. EducatorWorkMode ctx m
    => (forall x. m x -> Handler x)
    -> Server StudentAPI
studentAPIServer nat =
    hoistServer studentAPI nat servantHandlers

convertHandler
    :: EducatorContext
    -> EducatorRealMode a
    -> Handler a
convertHandler ctx handler =
    liftIO (runRIO ctx handler)
        `catch` (throwError . toServantErr)
        `catchAny` (throwError . unexpectedToServantErr)

serveStudentAPIReal :: HasEducatorConfig => NetworkAddress -> EducatorRealMode ()
serveStudentAPIReal addr = do
    logInfo $ "Serving Student API on "+|addr|+""
    eCtx <- ask
    serveWeb addr $
        serve studentAPI $
        studentAPIServer $
        convertHandler eCtx
