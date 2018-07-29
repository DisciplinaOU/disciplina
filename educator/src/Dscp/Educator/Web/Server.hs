-- | Functions to serve Student HTTP API

module Dscp.Educator.Web.Server
       ( serveStudentAPIReal
       , convertStudentApiHandler
       ) where

import Fmt ((+|), (|+))
import Loot.Log (logInfo)
import Servant (Handler, Server, hoistServer, serve, throwError)
import Servant.Generic (toServant)

import Dscp.Educator.Config (HasEducatorConfig)
import Dscp.Educator.Launcher (EducatorContext, EducatorRealMode, EducatorWorkMode)
import Dscp.Educator.Web.Bot.Handlers (addBotHandlers)
import Dscp.Educator.Web.Params (EducatorWebParams (..))
import Dscp.Educator.Web.Student.API (StudentAPI, studentAPI)
import Dscp.Educator.Web.Student.Error (toServantErr, unexpectedToServantErr)
import Dscp.Educator.Web.Student.Handlers (studentApiHandlers)
import Dscp.Launcher.Rio (runRIO)
import Dscp.Web (ServerParams (..), serveWeb)

studentAPIServer
    :: forall ctx m. EducatorWorkMode ctx m
    => (forall x. m x -> Handler x)
    -> Bool
    -> m (Server StudentAPI)
studentAPIServer nat withBot = do
    handlers <-
        bool identity (>>= addBotHandlers) withBot
        (pure studentApiHandlers)
    return $ hoistServer studentAPI nat (toServant handlers)

convertStudentApiHandler
    :: EducatorContext
    -> EducatorRealMode a
    -> Handler a
convertStudentApiHandler ctx handler =
    liftIO (runRIO ctx handler)
        `catch` (throwError . toServantErr)
        `catchAny` (throwError . unexpectedToServantErr)

serveStudentAPIReal :: HasEducatorConfig => EducatorWebParams -> EducatorRealMode ()
serveStudentAPIReal EducatorWebParams{..} = do
    let ServerParams{..} = ewpServerParams
    logInfo $ "Serving Student API on "+|spAddr|+""
    eCtx <- ask
    server <- studentAPIServer (convertStudentApiHandler eCtx) ewpWithBot
    serveWeb spAddr $ serve studentAPI server
