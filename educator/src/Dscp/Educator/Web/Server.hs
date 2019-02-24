{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

-- | Functions to serve Student HTTP API

module Dscp.Educator.Web.Server
       ( serveEducatorAPIsReal
       ) where

import Data.Proxy (Proxy (..))
import Fmt ((+|), (|+))
import Loot.Base.HasLens (lensOf)
import Loot.Log (logInfo)
import Network.HTTP.Types.Header (hAuthorization, hContentType)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant ((:<|>) (..), Context (..), Handler, ServantErr (..), Server, StdMethod (..), err405,
                hoistServerWithContext, serveWithContext)
import Servant.Auth.Server.Internal.ThrowAll (throwAll)
import Servant.Generic (toServant)
import Servant.Util (methodsCoveringAPI, serverWithLogging)
import UnliftIO (askUnliftIO)

import Dscp.Config
import Dscp.Core (mkAddr)
import Dscp.DB.SQL
import Dscp.Educator.Config
import Dscp.Educator.DB (existsStudent)
import Dscp.Educator.Launcher.Mode (EducatorNode, EducatorWorkMode)
import Dscp.Educator.Web.Auth
import Dscp.Educator.Web.Bot
import Dscp.Educator.Web.Educator (EducatorPublicKey (..), FullEducatorAPI,
                                   convertEducatorApiHandler, educatorApiHandlers,
                                   protectedEducatorAPI)
import Dscp.Educator.Web.Educator.Swagger
import Dscp.Educator.Web.Student (FullStudentAPI, StudentCheckAction (..), convertStudentApiHandler,
                                  protectedStudentAPI, studentApiHandlers)
import Dscp.Educator.Web.Student.Auth (mkStudentActionM)
import Dscp.Educator.Web.Student.Swagger
import Dscp.Resource.Keys (KeyResources, krPublicKey)
import Dscp.Web (buildServantLogConfig, serveWeb)
import Dscp.Web.Metrics (responseTimeMetric)
import Dscp.Web.Swagger.UI
import Dscp.Web.Types
import Dscp.Witness.Web

type EducatorWebAPI =
    FullEducatorAPI
    :<|>
    FullStudentAPI
    :<|>
    WitnessAPI

mkEducatorApiServer
    :: forall ctx m. EducatorWorkMode ctx m
    => (forall x. m x -> Handler x)
    -> NetworkAddress
    -> Server FullEducatorAPI
mkEducatorApiServer nat host =
    withSwaggerUI (educatorAPISwagger (Just host)) protectedEducatorAPI $
        hoistServerWithContext
            protectedEducatorAPI
            (Proxy :: Proxy '[EducatorPublicKey, NoAuthContext "educator"])
            nat
            (\() -> toServant educatorApiHandlers)

-- | Create these two:
-- 1. An action which checks whether or not the
--    student is a valid API user.
--    If bot is enabled, all students are allowed to use API and get automatically
--    registered.
-- 2. Server implementation itself.
mkStudentApiServer
    :: forall ctx m. EducatorWorkMode ctx m
    => (forall x. m x -> Handler x)
    -> NetworkAddress
    -> EducatorBotConfigRec
    -> m (StudentCheckAction, Server FullStudentAPI)
mkStudentApiServer nat host botConfig = case botConfig ^. tree #params . selection of
    "enabled" ->
        initializeBot (botConfig ^. tree #params . peekBranch #enabled) $ do
            let server = getServer $
                    \student -> addBotHandlers student $ studentApiHandlers student
            checkAction <- mkStudentActionM $ \pk -> do
                let student = mkAddr pk
                botProvideInitSetting student
                return True
            return (checkAction, server)
    "disabled" -> do
        let server = getServer studentApiHandlers
        checkAction <- mkStudentActionM $ \pk ->
              let addr = mkAddr pk
              in invoke $ existsStudent addr
        return (checkAction, server)
    other -> error $ "unknown EducatorBotConfig type: " <> fromString other
  where
    getServer handlers =
        withSwaggerUI (studentAPISwagger (Just host)) protectedStudentAPI $
            hoistServerWithContext
                protectedStudentAPI
                (Proxy :: Proxy '[StudentCheckAction, NoAuthContext "student"])
                nat
                (\student -> toServant $ handlers student)

-- | CORS is enabled to ease development for frontend team.
educatorCors :: Middleware
educatorCors = cors $ const $ Just $
    simpleCorsResourcePolicy
    { -- We use @Access-Control-Allow-Origin: *@ as soon as our JWT based
      -- authentication prevents CSRF attacks, and API is public anyway.
      corsOrigins = Nothing
    , corsRequestHeaders = [hContentType, hAuthorization]
    , corsMethods = methodsCoveringAPI @['GET, 'POST, 'PUT, 'DELETE] @EducatorWebAPI
    }

serveEducatorAPIsReal :: EducatorWorkMode ctx m => Bool -> m ()
serveEducatorAPIsReal withWitnessApi = do
    let webCfg = educatorConfig ^. sub #educator . sub #api
        serverAddress     = webCfg ^. sub #serverParams . option #addr
        botConfig         = webCfg ^. sub #botConfig
        educatorAPINoAuth = webCfg ^. option #educatorAPINoAuth
        studentAPINoAuth  = webCfg ^. option #studentAPINoAuth
    unliftIO <- askUnliftIO

    educatorKeyResources <- view (lensOf @(KeyResources EducatorNode))
    (studentCheckAction, studentApiServer) <-
          mkStudentApiServer (convertStudentApiHandler unliftIO) serverAddress botConfig

    let educatorPublicKey = EducatorPublicKey $ educatorKeyResources ^. krPublicKey
    let srvCtx = educatorPublicKey :. educatorAPINoAuth :.
                 studentCheckAction :. studentAPINoAuth :.
                 EmptyContext

    logInfo $ "Serving Student API on "+|serverAddress|+""
    lc <- buildServantLogConfig (<> "web")
    let educatorApiServer =
          mkEducatorApiServer (convertEducatorApiHandler unliftIO) serverAddress
    let witnessApiServer = if withWitnessApi
          then mkWitnessAPIServer (convertWitnessHandler unliftIO)
          else throwAll err405{ errBody = "Witness API disabled at this port" }
    let metricsEndpoint = witnessConfig ^. sub #witness . option #metricsEndpoint
    serveWeb serverAddress $
      responseTimeMetric metricsEndpoint $
      educatorCors $
      serverWithLogging lc (Proxy @EducatorWebAPI) $ \api ->
      serveWithContext api srvCtx $
          educatorApiServer
          :<|>
          studentApiServer
          :<|>
          witnessApiServer
