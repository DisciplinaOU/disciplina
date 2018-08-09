{-# LANGUAGE TypeOperators #-}

-- | Functions to serve Student HTTP API

module Dscp.Educator.Web.Server
       ( serveStudentAPIReal
       ) where

import Fmt ((+|), (|+))
import Loot.Log (logInfo)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant ((:<|>) (..), Handler, Server, hoistServer, serve)
import Servant.Generic (toServant)

import Dscp.Educator.Config (HasEducatorConfig)
import Dscp.Educator.Launcher (EducatorRealMode, EducatorWorkMode)
import Dscp.Educator.Web.Bot (EducatorBotSwitch (..), addBotHandlers)
import Dscp.Educator.Web.Educator (EducatorAPI, convertEducatorApiHandler, educatorAPI,
                                   educatorApiHandlers)
import Dscp.Educator.Web.Params (EducatorWebParams (..))
import Dscp.Educator.Web.Student (StudentAPI, convertStudentApiHandler, studentAPI,
                                  studentApiHandlers)
import Dscp.Web (ServerParams (..), serveWeb)

type EducatorWebAPI =
    EducatorAPI
    :<|>
    StudentAPI

mkEducatorApiServer
    :: forall ctx m. EducatorWorkMode ctx m
    => (forall x. m x -> Handler x)
    -> Server EducatorAPI
mkEducatorApiServer nat =
    hoistServer educatorAPI nat (toServant educatorApiHandlers)

mkStudentApiServer
    :: forall ctx m. EducatorWorkMode ctx m
    => (forall x. m x -> Handler x)
    -> EducatorBotSwitch
    -> m (Server StudentAPI)
mkStudentApiServer nat botSwitch = do
    totalHandlers <- addBot studentApiHandlers
    return $ hoistServer studentAPI nat (toServant totalHandlers)
  where
    addBot cur = case botSwitch of
        EducatorBotOff       -> pure cur
        EducatorBotOn params -> addBotHandlers params cur

serveStudentAPIReal :: HasEducatorConfig => EducatorWebParams -> EducatorRealMode ()
serveStudentAPIReal EducatorWebParams{..} = do
    let ServerParams{..} = ewpServerParams
    logInfo $ "Serving educator APIs on "+|spAddr|+""
    eCtx <- ask
    let educatorApiServer = mkEducatorApiServer (convertEducatorApiHandler eCtx)
    studentApiServer <- mkStudentApiServer (convertStudentApiHandler eCtx) ewpBotParams
    serveWeb spAddr $
        simpleCors $
        serve (Proxy @EducatorWebAPI) $
            educatorApiServer
            :<|>
            studentApiServer
