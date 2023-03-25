{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

-- | Functions to serve Student HTTP API

module Dscp.Educator.Web.Server
       ( serveEducatorAPIsReal
       ) where

import Universum

import Fmt ((+|), (|+))
import Loot.Log (logInfo)
import Network.HTTP.Types.Header (hAuthorization, hContentType)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant (Context (..), Handler, Server, StdMethod (..), hoistServerWithContext,
                serveWithContext, (:<|>) (..))
import Servant.API.Generic (toServant)
import Servant.Util (methodsCoveringAPI, serverWithLogging)
import UnliftIO (askUnliftIO)

import Dscp.Config
import Dscp.Educator.Config
import Dscp.Educator.Launcher.Mode (EducatorWorkMode)
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
import Dscp.Web (buildServantLogConfig, serveWeb)
import Dscp.Web.Swagger.UI
import Dscp.Web.Types

type EducatorWebAPI =
    FullEducatorAPI
    :<|>
    FullStudentAPI

mkEducatorApiServer
    :: forall ctx m. EducatorWorkMode ctx m
    => (forall x. m x -> Handler x)
    -> NetworkAddress
    -> Server FullEducatorAPI
mkEducatorApiServer nat host =
    withSwaggerUI protectedEducatorAPI (educatorAPISwagger (Just host)) $
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
            checkAction <- mkStudentActionM $ \_student -> do
                -- there is no bot
                -- botProvideInitSetting student
                return True
            return (checkAction, server)
    "disabled" -> do
        let server = getServer studentApiHandlers
        checkAction <- mkStudentActionM $ \_addr -> return True
        return (checkAction, server)
    other -> error $ "unknown EducatorBotConfig type: " <> fromString other
  where
    getServer handlers =
        withSwaggerUI Proxy (studentAPISwagger (Just host)) $
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

serveEducatorAPIsReal :: EducatorWorkMode ctx m => m a
serveEducatorAPIsReal = do
    let webCfg = educatorConfig ^. sub #educator . sub #api
        serverAddress     = webCfg ^. sub #serverParams . option #addr
        botConfig         = webCfg ^. sub #botConfig
        educatorAPINoAuth = webCfg ^. option #educatorAPINoAuth
        studentAPINoAuth  = webCfg ^. option #studentAPINoAuth
    unliftIO <- askUnliftIO

    (studentCheckAction, studentApiServer) <-
          mkStudentApiServer (convertStudentApiHandler unliftIO) serverAddress botConfig
    whenNoAuth studentAPINoAuth $
        liftIO . void . runStudentCheckAction studentCheckAction

    -- TODO: removed this authentication method because keyResources are obsolete.
    -- Trying to authenticate with token with educator running will always fail now.
    -- educatorKeyResources <- view (lensOf @(KeyResources EducatorNode))
    -- let educatorPublicKey = EducatorPublicKey $ educatorKeyResources ^. krPublicKey
    let educatorPublicKey = EducatorPublicKey $ error "Educator PubKey authentication is removed"
    let srvCtx = educatorPublicKey :. educatorAPINoAuth :.
                 studentCheckAction :. studentAPINoAuth :.
                 EmptyContext

    logInfo $ "Serving Student API on "+|serverAddress|+""
    lc <- buildServantLogConfig (<> "web")
    let educatorApiServer =
          mkEducatorApiServer (convertEducatorApiHandler unliftIO) serverAddress

    serveWeb serverAddress $
      educatorCors $
      serverWithLogging lc (Proxy @EducatorWebAPI) $ \_ ->
      serveWithContext (Proxy @EducatorWebAPI) srvCtx $
          educatorApiServer
          :<|>
          studentApiServer
