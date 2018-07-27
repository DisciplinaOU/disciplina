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
import Dscp.Educator.Web.Student.Handlers (studentApiHandlers)
import Dscp.Launcher.Rio (runRIO)
import Dscp.Web (ServerParams (..), serveWeb)

studentAPIServer
    :: forall ctx m. EducatorWorkMode ctx m
    => (forall x. m x -> Handler x)
    -> Server StudentAPI
studentAPIServer nat =
    hoistServer studentAPI nat studentApiHandlers

convertHandler
    :: EducatorContext
    -> EducatorRealMode a
    -> Handler a
convertHandler ctx handler =
    liftIO (runRIO ctx handler)
        `catch` (throwError . toServantErr)
        `catchAny` (throwError . unexpectedToServantErr)

serveStudentAPIReal :: HasEducatorConfig => ServerParams -> EducatorRealMode ()
serveStudentAPIReal ServerParams{..} = do
    logInfo $ "Serving Student API on "+|spAddr|+""
    eCtx <- ask
    serveWeb spAddr $
        serve studentAPI $
        studentAPIServer $
        convertHandler eCtx
