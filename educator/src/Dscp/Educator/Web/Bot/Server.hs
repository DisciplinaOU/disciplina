-- | Serving student API with bot.

module Dscp.Educator.Web.Bot.Server
       ( serveStudentAPIWithBotReal
       ) where

import Fmt ((+|), (|+))
import Loot.Log (logInfo)
import Servant (hoistServer, serve)
import Servant.Generic (toServant)

import Dscp.Educator.Config (HasEducatorConfig)
import Dscp.Educator.Launcher (EducatorRealMode)
import Dscp.Educator.Web.Bot.Handlers (addBotHandlers)
import Dscp.Educator.Web.Student.API (studentAPI)
import Dscp.Educator.Web.Student.Handlers (studentApiHandlers)
import Dscp.Educator.Web.Student.Server (convertStudentApiHandler)
import Dscp.Web (ServerParams (..), serveWeb)

serveStudentAPIWithBotReal
    :: HasEducatorConfig
    => ServerParams -> EducatorRealMode ()
serveStudentAPIWithBotReal ServerParams{..} = do
    logInfo $ "Serving Student API on "+|spAddr|+""
    eCtx <- ask
    studentApiWithBotHandlers <- addBotHandlers studentApiHandlers
    let server = hoistServer studentAPI (convertStudentApiHandler eCtx)
                 (toServant studentApiWithBotHandlers)
    serveWeb spAddr $ serve studentAPI server
