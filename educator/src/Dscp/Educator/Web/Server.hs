{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

-- | Functions to serve Student HTTP API

module Dscp.Educator.Web.Server
       ( serveEducatorAPIsReal
       ) where

import Data.Proxy (Proxy (..))
import Data.Reflection (Reifies, reify)
import Fmt ((+|), (|+))
import Loot.Base.HasLens (lensOf)
import Loot.Config (option, sub)
import Loot.Log (logInfo)
import Network.HTTP.Types.Header (hAuthorization, hContentType)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant ((:<|>) (..), Context (..), Handler, ServantErr (..), Server, err405,
                hoistServerWithContext, serveWithContext)
import Servant.Auth.Server.Internal.ThrowAll (throwAll)
import Servant.Generic (toServant)
import UnliftIO (askUnliftIO)

import Dscp.Core (mkAddr)
import Dscp.DB.SQLite (SQLiteDB, invoke)
import Dscp.Educator.Config
import Dscp.Educator.DB (existsStudent)
import Dscp.Educator.Launcher.Mode (EducatorNode, EducatorWorkMode)
import Dscp.Educator.Web.Auth
import Dscp.Educator.Web.Bot (EducatorBotParams (..), addBotHandlers, initializeBot)
import Dscp.Educator.Web.Educator (EducatorPublicKey (..), ProtectedEducatorAPI,
                                   convertEducatorApiHandler, educatorApiHandlers,
                                   protectedEducatorAPI)
import Dscp.Educator.Web.Student (ProtectedStudentAPI, StudentCheckAction (..),
                                  convertStudentApiHandler, studentAPI, studentApiHandlers)
import Dscp.Resource.Keys (KeyResources, krPublicKey)
import Dscp.Util.Servant (LoggingApi, ServantLogConfig (..))
import Dscp.Web (ServerParams (..), buildServantLogConfig, serveWeb)
import Dscp.Web.Metrics (responseTimeMetric)
import Dscp.Witness.Web

type EducatorWebAPI =
    ProtectedEducatorAPI
    :<|>
    ProtectedStudentAPI
    :<|>
    WitnessAPI

mkEducatorApiServer
    :: forall ctx m. EducatorWorkMode ctx m
    => (forall x. m x -> Handler x)
    -> Server ProtectedEducatorAPI
mkEducatorApiServer nat =
    hoistServerWithContext
        protectedEducatorAPI
        (Proxy :: Proxy '[EducatorPublicKey, NoAuthContext "educator"])
        nat
        (\() -> toServant educatorApiHandlers)

mkStudentApiServer
    :: forall ctx m. EducatorWorkMode ctx m
    => (forall x. m x -> Handler x)
    -> EducatorBotParams
    -> m (Server ProtectedStudentAPI)
mkStudentApiServer nat botParams =
    if ebpEnabled botParams
    then initializeBot botParams $ return $ \student ->
        getServer . addBotHandlers student .
        studentApiHandlers $ student
    else return $ getServer . studentApiHandlers
  where
    getServer handlers = hoistServerWithContext
        studentAPI
        (Proxy :: Proxy '[StudentCheckAction])
        nat
        (toServant handlers)

-- | Create an action which checks whether or not the
-- student is a valid API user.
-- If bot is enabled, all students are allowed to use API.
createStudentCheckAction
    :: forall ctx m. EducatorWorkMode ctx m
    => EducatorBotParams
    -> m StudentCheckAction
createStudentCheckAction EducatorBotParams {..}
    | ebpEnabled = return . StudentCheckAction . const $ pure True
    | otherwise = do
          db <- view (lensOf @SQLiteDB)
          return . StudentCheckAction $ \pk ->
              let addr = mkAddr pk
              in runReaderT (invoke $ existsStudent addr) db

-- | CORS is enabled to ease development for frontend team.
educatorCors :: Middleware
educatorCors = cors $ const $ Just $
    simpleCorsResourcePolicy
    { -- We use @Access-Control-Allow-Origin: *@ as soon as our JWT based
      -- authentication prevents CSRF attacks, and API is public anyway.
      corsOrigins = Nothing
    , corsRequestHeaders = [hContentType, hAuthorization]
    , corsMethods = ["GET", "POST", "DELETE"]
    }

serveEducatorAPIsReal :: EducatorWorkMode ctx m => Bool -> m ()
serveEducatorAPIsReal withWitnessApi = do
    let webCfg = educatorConfig ^. sub #educator . sub #api
        ServerParams{..}  = webCfg ^. option #serverParams
        botParams         = webCfg ^. option #botParams
        educatorAPINoAuth = webCfg ^. option #educatorAPINoAuth
        studentAPINoAuth  = webCfg ^. option #studentAPINoAuth

    educatorKeyResources <- view (lensOf @(KeyResources EducatorNode))
    studentCheckAction <- createStudentCheckAction botParams
    let educatorPublicKey = EducatorPublicKey $ educatorKeyResources ^. krPublicKey
    let srvCtx = educatorPublicKey :. educatorAPINoAuth :.
                 studentCheckAction :. studentAPINoAuth :.
                 EmptyContext

    logInfo $ "Serving Student API on "+|spAddr|+""
    unliftIO <- askUnliftIO
    lc <- buildServantLogConfig (<> "web")
    let educatorApiServer = mkEducatorApiServer (convertEducatorApiHandler unliftIO)
    studentApiServer <- mkStudentApiServer (convertStudentApiHandler unliftIO) botParams
    let witnessApiServer = if withWitnessApi
          then mkWitnessAPIServer (convertWitnessHandler unliftIO)
          else throwAll err405{ errBody = "Witness API disabled at this port" }
    let metricsEndpoint = witnessConfig ^. sub #witness . option #metricsEndpoint
    serveWeb spAddr $
      responseTimeMetric metricsEndpoint $
      educatorCors $
      reify lc $ \logConfigP ->
      serveWithContext (servedApi logConfigP) srvCtx $
          educatorApiServer
          :<|>
          studentApiServer
          :<|>
          witnessApiServer
      where
          servedApi
            :: Reifies config ServantLogConfig
            => Proxy config -> Proxy (LoggingApi config EducatorWebAPI)
          servedApi _ = Proxy
