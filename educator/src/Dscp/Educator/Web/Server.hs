{-# LANGUAGE TypeOperators #-}

-- | Functions to serve Student HTTP API

module Dscp.Educator.Web.Server
       ( serveStudentAPIReal
       ) where

import Fmt ((+|), (|+))
import Loot.Log (logInfo)
import Servant ((:<|>) (..), (:>), Handler, Server, hoistServer, serve)
import Servant.Generic (toServant)

import Dscp.Educator.Config (HasEducatorConfig)
import Dscp.Educator.Launcher (EducatorRealMode, EducatorWorkMode)
import Dscp.Educator.Web.Bot (addBotHandlers)
import Dscp.Educator.Web.Educator (EducatorAPI, convertEducatorApiHandler, educatorAPI,
                                   educatorApiHandlers)
import Dscp.Educator.Web.Params (EducatorWebParams (..))
import Dscp.Educator.Web.Student (StudentAPI, convertStudentApiHandler, studentAPI,
                                  studentApiHandlers)
import Dscp.Web (ServerParams (..), serveWeb)

type EducatorWebAPI = "api" :> (
    "educator" :> EducatorAPI
    :<|>
    "student" :> StudentAPI
  )

mkEducatorApiServer
    :: forall ctx m. EducatorWorkMode ctx m
    => (forall x. m x -> Handler x)
    -> Server EducatorAPI
mkEducatorApiServer nat =
    hoistServer educatorAPI nat (toServant educatorApiHandlers)

mkStudentApiServer
    :: forall ctx m. EducatorWorkMode ctx m
    => (forall x. m x -> Handler x)
    -> Bool
    -> m (Server StudentAPI)
mkStudentApiServer nat withBot = do
    studentAndBotHandlers <-
        bool identity (>>= addBotHandlers) withBot
        (pure studentApiHandlers)
    return $ hoistServer studentAPI nat (toServant studentAndBotHandlers)

serveStudentAPIReal :: HasEducatorConfig => EducatorWebParams -> EducatorRealMode ()
serveStudentAPIReal EducatorWebParams{..} = do
    let ServerParams{..} = ewpServerParams
    logInfo $ "Serving Student API on "+|spAddr|+""
    eCtx <- ask
    let educatorApiServer = mkEducatorApiServer (convertEducatorApiHandler eCtx)
    studentApiServer <- mkStudentApiServer (convertStudentApiHandler eCtx) ewpWithBot
    serveWeb spAddr $ serve (Proxy @EducatorWebAPI) $
         educatorApiServer
         :<|>
         studentApiServer
