{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

-- | Functions to serve Student HTTP API

module Dscp.MultiEducator.Web.Server
       ( serveEducatorAPIsReal
       ) where

import Data.Proxy (Proxy (..))
import Fmt ((+|), (|+))
import Loot.Log (logInfo)
import Network.HTTP.Types.Header (hAuthorization, hContentType)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant ((:<|>) (..), Context (..), Handler, ServantErr (..), Server, ServerT,
                StdMethod (..), err405, hoistServer, hoistServerWithContext, serveWithContext)
import Servant.Auth.Server.Internal.ThrowAll (throwAll)
import Servant.Generic (toServant)
import Servant.Util (methodsCoveringAPI, serverWithLogging)
import UnliftIO (askUnliftIO)

import Dscp.Config
import Dscp.Educator.Web.Auth
import Dscp.Educator.Web.Educator (RawEducatorAPI, convertEducatorApiHandler, educatorApiHandlers,
                                   rawEducatorAPI)
import Dscp.Educator.Web.Student (StudentCheckAction (..), convertStudentApiHandler,
                                  protectedStudentAPI, studentApiHandlers)
import Dscp.MultiEducator.Config
import Dscp.MultiEducator.Launcher.Educator.Load
import Dscp.MultiEducator.Launcher.Mode (MultiCombinedWorkMode, MultiEducatorWorkMode)
import Dscp.MultiEducator.Launcher.Params (MultiEducatorAAAConfigRec)
import Dscp.MultiEducator.Web.Educator
import Dscp.MultiEducator.Web.Swagger
import Dscp.Web (buildServantLogConfig, serveWeb)
import Dscp.Web.Metrics (responseTimeMetric)
import Dscp.Web.Swagger.UI
import Dscp.Web.Types
import Dscp.Witness.Web

type MultiEducatorWebAPI =
    MultiEducatorAPI
    :<|>
    MultiStudentAPI
    :<|>
    FullCertificatesAPI
    :<|>
    WitnessAPI

mkEducatorApiServer'
    :: forall ctx m. MultiEducatorWorkMode ctx m
    => EducatorAuthLogin
    -> ServerT RawEducatorAPI m
mkEducatorApiServer' educatorAuthLogin =
    hoistServerWithContext
        rawEducatorAPI
        (Proxy :: Proxy '[])
        (\x -> lookupEducator educatorAuthLogin >>= \c -> normalToMulti c x)
        (toServant educatorApiHandlers)

mkMultiEducatorApiServer
    :: forall ctx m. MultiEducatorWorkMode ctx m
    => MultiEducatorAAAConfigRec
    -> (forall x. m x -> Handler x)
    -> NetworkAddress
    -> Server MultiEducatorAPI
mkMultiEducatorApiServer _aaaConfig nat host =
    withSwaggerUI protectedMultiEducatorAPI (multiEducatorAPISwagger (Just host)) $
        hoistServerWithContext
            protectedMultiEducatorAPI
            (Proxy :: Proxy '[MultiEducatorPublicKey, NoAuthContext "multi-educator"])
            nat
            mkEducatorApiServer'

mkStudentApiServer
    :: forall ctx m. MultiEducatorWorkMode ctx m
    => (forall x. m x -> Handler x)
    -> NetworkAddress
    -> Server MultiStudentAPI
mkStudentApiServer nat host =
    withSwaggerUI protectedMultiStudentAPI (multiStudentAPISwagger (Just host)) $
        \login ->
        let ealogin = educatorAuthLoginSimple login
        in hoistServerWithContext
                protectedStudentAPI
                (Proxy :: Proxy '[StudentCheckAction, NoAuthContext "student"])
                (\x -> nat $ lookupEducator ealogin >>= \c -> normalToMulti c x)
                (\student -> toServant $ studentApiHandlers student)

mkCertificatesApiServer
    :: forall ctx m. MultiEducatorWorkMode ctx m
    => (forall x. m x -> Handler x)
    -> NetworkAddress
    -> Server FullCertificatesAPI
mkCertificatesApiServer nat host =
    withSwaggerUI certificatesAPI (certificatesAPISwagger (Just host)) $
        hoistServer certificatesAPI nat $ toServant certificatesApiHandlers

-- | Create an action which checks whether or not the
-- student is a valid API user.
-- If bot is enabled, all students are allowed to use API.
createStudentCheckAction
    :: forall ctx m. MultiEducatorWorkMode ctx m
    => m StudentCheckAction
createStudentCheckAction =
    return . StudentCheckAction . const $ pure True
{-
createStudentCheckAction = do
    db <- view (lensOf @SQL)
    return . StudentCheckAction $ \pk ->
        let addr = mkAddr pk
        in runReaderT (invoke $ existsStudent addr) db
-}

-- | CORS is enabled to ease development for frontend team.
educatorCors :: Middleware
educatorCors = cors $ const $ Just $
    simpleCorsResourcePolicy
    { -- We use @Access-Control-Allow-Origin: *@ as soon as our JWT based
      -- authentication prevents CSRF attacks, and API is public anyway.
      corsOrigins = Nothing
    , corsRequestHeaders = [hContentType, hAuthorization]
    , corsMethods = methodsCoveringAPI @['GET, 'POST, 'PUT, 'DELETE] @MultiEducatorWebAPI
    }

serveEducatorAPIsReal :: MultiCombinedWorkMode ctx m => Bool -> m a
serveEducatorAPIsReal withWitnessApi = do
    let webCfg = multiEducatorConfig ^. sub #educator . sub #api
        serverAddress     = webCfg ^. sub #serverParams . option #addr
        studentAPINoAuth  = webCfg ^. option #studentAPINoAuth
        educatorAPINoAuth = webCfg ^. option #multiEducatorAPINoAuth
        aaaSettings       = multiEducatorConfig ^. sub #educator . sub #aaa
        metricsEndpoint   = witnessConfig ^. sub #witness . option #metricsEndpoint

    studentCheckAction <- createStudentCheckAction
    let educatorPublicKey = aaaSettings ^. option #publicKey
        srvCtx =
            educatorPublicKey :.
            educatorAPINoAuth :.
            studentCheckAction :.
            studentAPINoAuth :.
            EmptyContext

    logInfo $ "Serving Student API on "+|serverAddress|+""
    unliftIO <- askUnliftIO
    let educatorApiServer =
            mkMultiEducatorApiServer
                aaaSettings
                (convertEducatorApiHandler unliftIO)
                serverAddress

        certificatesApiServer =
            mkCertificatesApiServer
                (convertEducatorApiHandler unliftIO)
                serverAddress

        studentApiServer =
            mkStudentApiServer
                (convertStudentApiHandler unliftIO)
                serverAddress

        witnessApiServer = if withWitnessApi
          then mkWitnessAPIServer (convertWitnessHandler unliftIO)
          else throwAll err405{ errBody = "Witness API disabled at this port" }

    lc <- buildServantLogConfig (<> "web")
    serveWeb serverAddress $
        responseTimeMetric metricsEndpoint $
        educatorCors $
        serverWithLogging lc (Proxy @MultiEducatorWebAPI) $ \api ->
        serveWithContext api srvCtx $
            educatorApiServer
            :<|>
            studentApiServer
            :<|>
            certificatesApiServer
            :<|>
            witnessApiServer
