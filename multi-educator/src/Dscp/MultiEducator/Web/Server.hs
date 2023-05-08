{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

-- | Functions to serve Student HTTP API

module Dscp.MultiEducator.Web.Server
       ( serveEducatorAPIsReal
       ) where

import Universum

import Fmt ((+|), (|+))
import Loot.Log (logInfo)
import Network.HTTP.Types.Header (hAuthorization, hContentType)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant (Context (..), Handler, Server, ServerT, StdMethod (..), hoistServer,
                hoistServerWithContext, serveWithContext, (:<|>) (..))
import Servant.API.Generic (toServant)
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
import Dscp.Web.Swagger.UI
import Dscp.Web.Types

type MultiEducatorWebAPI =
    MultiEducatorAPI
    :<|>
    MultiStudentAPI
    :<|>
    FullCertificatesAPI

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
mkMultiEducatorApiServer _aaaConfig nat _host =
    withSwaggerUI protectedMultiEducatorAPI (multiEducatorAPISwagger Nothing) $
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
mkStudentApiServer nat _host =
    withSwaggerUI protectedMultiStudentAPI (multiStudentAPISwagger Nothing) $
        \login ->
        let ealogin = educatorAuthLoginSimple login
        in hoistServerWithContext
                protectedStudentAPI
                (Proxy :: Proxy '[StudentCheckAction, NoAuthContext "student"])
                (\x -> nat $ lookupEducator ealogin >>= \c -> normalToMulti c x)
                (toServant . studentApiHandlers)

mkCertificatesApiServer
    :: forall ctx m. MultiEducatorWorkMode ctx m
    => (forall x. m x -> Handler x)
    -> NetworkAddress
    -> Server FullCertificatesAPI
mkCertificatesApiServer nat _host =
    withSwaggerUI certificatesAPI (certificatesAPISwagger Nothing) $
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

serveEducatorAPIsReal :: MultiCombinedWorkMode ctx m => m a
serveEducatorAPIsReal = do
    let webCfg = multiEducatorConfig ^. sub #educator . sub #api
        serverAddress     = webCfg ^. sub #serverParams . option #addr
        studentAPINoAuth  = webCfg ^. option #studentAPINoAuth
        educatorAPINoAuth = webCfg ^. option #multiEducatorAPINoAuth
        aaaSettings       = multiEducatorConfig ^. sub #educator . sub #aaa

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

    lc <- buildServantLogConfig (<> "web")
    serveWeb serverAddress $
        educatorCors $
        serverWithLogging lc (Proxy @MultiEducatorWebAPI) $ \_ ->
        serveWithContext (Proxy @MultiEducatorWebAPI) srvCtx $
            educatorApiServer
            :<|>
            studentApiServer
            :<|>
            certificatesApiServer
