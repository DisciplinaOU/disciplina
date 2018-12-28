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
import Loot.Log (logInfo)
import Network.HTTP.Types.Header (hAuthorization, hContentType)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant ((:<|>) (..), Context (..), Handler, ServantErr (..), Server, StdMethod (..), err405,
                hoistServerWithContext, serveWithContext)
import Servant.Auth.Server.Internal.ThrowAll (throwAll)
import Servant.Generic (toServant)
import Servant.Util (LoggingApi, ServantLogConfig (..), methodsCoveringAPI)
import UnliftIO (askUnliftIO)

import Dscp.Config
import Dscp.Core (mkAddr)
import Dscp.DB.SQL (SQL, invoke)
import Dscp.Educator.Config
import Dscp.Educator.DB (existsStudent)
import Dscp.Educator.Launcher.Mode (EducatorNode, EducatorWorkMode)
import Dscp.Educator.Web.Auth
import Dscp.Educator.Web.Bot (EducatorBotConfigRec, addBotHandlers, initializeBot)
import Dscp.Educator.Web.Educator (EducatorPublicKey (..), ProtectedEducatorAPI,
                                   convertEducatorApiHandler, educatorApiHandlers,
                                   protectedEducatorAPI)
import Dscp.Educator.Web.Student (ProtectedStudentAPI, StudentCheckAction (..),
                                  convertStudentApiHandler, rawStudentAPI, studentApiHandlers)
import Dscp.Resource.Keys (KeyResources, krPublicKey)
import Dscp.Web (buildServantLogConfig, serveWeb)
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
    -> EducatorBotConfigRec
    -> m (Server ProtectedStudentAPI)
mkStudentApiServer nat botConfig = case botConfig ^. tree #params . selection of
    "enabled" -> initializeBot (botConfig ^. tree #params . peekBranch #enabled) $
        return $ \student ->
            getServer . addBotHandlers student .
            studentApiHandlers $ student
    "disabled" -> return $ getServer . studentApiHandlers
    sel -> error $ "unknown EducatorBotConfig type: " <> fromString sel
  where
    getServer handlers = hoistServerWithContext
        rawStudentAPI
        (Proxy :: Proxy '[StudentCheckAction])
        nat
        (toServant handlers)

-- | Create an action which checks whether or not the
-- student is a valid API user.
-- If bot is enabled, all students are allowed to use API.
createStudentCheckAction
    :: forall ctx m. EducatorWorkMode ctx m
    => EducatorBotConfigRec
    -> m StudentCheckAction
createStudentCheckAction botConfig = case botConfig ^. tree #params . selection of
    "enabled"  -> return . StudentCheckAction . const $ pure True
    "disabled" -> do
          db <- view (lensOf @SQL)
          return . StudentCheckAction $ \pk ->
              let addr = mkAddr pk
              in runReaderT (invoke $ existsStudent addr) db
    sel -> error $ "unknown EducatorBotConfig type: " <> fromString sel

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

    educatorKeyResources <- view (lensOf @(KeyResources EducatorNode))
    studentCheckAction <- createStudentCheckAction botConfig
    let educatorPublicKey = EducatorPublicKey $ educatorKeyResources ^. krPublicKey
    let srvCtx = educatorPublicKey :. educatorAPINoAuth :.
                 studentCheckAction :. studentAPINoAuth :.
                 EmptyContext

    logInfo $ "Serving Student API on "+|serverAddress|+""
    unliftIO <- askUnliftIO
    lc <- buildServantLogConfig (<> "web")
    let educatorApiServer = mkEducatorApiServer (convertEducatorApiHandler unliftIO)
    studentApiServer <- mkStudentApiServer (convertStudentApiHandler unliftIO) botConfig
    let witnessApiServer = if withWitnessApi
          then mkWitnessAPIServer (convertWitnessHandler unliftIO)
          else throwAll err405{ errBody = "Witness API disabled at this port" }
    let metricsEndpoint = witnessConfig ^. sub #witness . option #metricsEndpoint
    serveWeb serverAddress $
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
