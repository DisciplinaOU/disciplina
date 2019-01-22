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
                StdMethod (..), err401, err405, hoistServerWithContext, serveWithContext)
import Servant.Auth.Server (AuthResult (..), CookieSettings, JWTSettings, defaultCookieSettings,
                            defaultJWTSettings, generateKey)
import Servant.Auth.Server.Internal.ThrowAll (throwAll)
import Servant.Generic (toServant)
import Servant.Util (methodsCoveringAPI)
import UnliftIO (askUnliftIO)

import Dscp.Config
import Dscp.Educator.Web.Auth
import Dscp.Educator.Web.Bot
import Dscp.Educator.Web.Educator (EducatorAPI, convertEducatorApiHandler, educatorAPI,
                                   educatorApiHandlers)
import Dscp.Educator.Web.Student (ProtectedStudentAPI, StudentCheckAction (..),
                                  convertStudentApiHandler, studentAPI, studentApiHandlers)
import Dscp.MultiEducator.Config
import Dscp.MultiEducator.Launcher.Mode (MultiCombinedWorkMode, MultiEducatorWorkMode,
                                         lookupEducator, normalToMulti)
import Dscp.MultiEducator.Launcher.Resource (EducatorCtxWithCfg (..))
import Dscp.MultiEducator.Web.Educator (MultiEducatorAPI, MultiStudentAPI, multiEducatorAPI,
                                        multiEducatorApiHandlers, multiStudentAPI)
import Dscp.MultiEducator.Web.Educator.Auth
import Dscp.Web (serveWeb)
import Dscp.Web.Metrics (responseTimeMetric)
import Dscp.Witness.Web

type EducatorWebAPI =
    MultiEducatorAPI
    :<|>
    MultiStudentAPI
    :<|>
    WitnessAPI

mkEducatorApiServer'
    :: forall ctx m. MultiEducatorWorkMode ctx m
    => Text
    -> ServerT EducatorAPI m
mkEducatorApiServer' login =
    hoistServerWithContext
        educatorAPI
        (Proxy :: Proxy '[])
        (\x -> lookupEducator login >>= \case
            Just c@(EducatorCtxWithCfg _) -> normalToMulti c x
            Nothing -> throwM err401
        )
        (toServant educatorApiHandlers)

mkMultiEducatorApiServer
    :: forall ctx m. MultiEducatorWorkMode ctx m
    => JWTSettings
    -> CookieSettings
    -> (forall x. m x -> Handler x)
    -> Server MultiEducatorAPI
mkMultiEducatorApiServer jwtSet cookieSet nat =
    hoistServerWithContext
        multiEducatorAPI
        (Proxy :: Proxy '[JWTSettings, CookieSettings])
        nat
        (toServant (multiEducatorApiHandlers jwtSet cookieSet) :<|> \case
            (Authenticated (EducatorAuthData login)) -> mkEducatorApiServer' login
            _ -> error "401" -- throwAll causes overlapping instances here, not sure how to solve this

        )

mkStudentApiServer'
    :: forall ctx m. MultiEducatorWorkMode ctx m
    => (forall x. m x -> Handler x)
    -> EducatorBotConfigRec
    -> m (Text -> Server ProtectedStudentAPI)
mkStudentApiServer' nat botConfig = do
    case botConfig ^. tree #params . selection of
      _ {-EducatorBotOff-} -> return $ \login -> getServer login . studentApiHandlers
      -- To initilialize the bot we need a database, how do we choose a database?
      {-EducatorBotOn params -> initializeBot params $ do
        return $ (\student -> getServer . addBotHandlers student . studentApiHandlers $ student)-}
  where
    getServer login handlers = hoistServerWithContext
        studentAPI
        (Proxy :: Proxy '[StudentCheckAction])
        (\x -> nat $ lookupEducator login >>= \case
            Just c@(EducatorCtxWithCfg _) -> normalToMulti c x
            Nothing -> throwM err401
        )
        (toServant handlers)

mkStudentApiServer
    :: forall ctx m. MultiEducatorWorkMode ctx m
    => (forall x. m x -> Handler x)
    -> EducatorBotConfigRec
    -> m (Server MultiStudentAPI)
mkStudentApiServer nat botParams = do
    studApi <- mkStudentApiServer' nat botParams
    return $ hoistServerWithContext
        multiStudentAPI
        (Proxy :: Proxy '[StudentCheckAction, NoAuthContext "student"])
        id
        studApi

-- | Create an action which checks whether or not the
-- student is a valid API user.
-- If bot is enabled, all students are allowed to use API.
createStudentCheckAction
    :: forall ctx m. MultiEducatorWorkMode ctx m
    => EducatorBotConfigRec
    -> m StudentCheckAction
createStudentCheckAction _ =
    return . StudentCheckAction . const $ pure True
{-
createStudentCheckAction EducatorBotOff = do
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
    , corsMethods = methodsCoveringAPI @['GET, 'POST, 'PUT, 'DELETE] @EducatorWebAPI
    }

serveEducatorAPIsReal :: MultiCombinedWorkMode ctx m => Bool -> m ()
serveEducatorAPIsReal withWitnessApi = do
    let webCfg = multiEducatorConfig ^. sub #educator . sub #api
        serverAddress     = webCfg ^. sub #serverParams . option #addr
        botConfig         = webCfg ^. sub #botConfig
        studentAPINoAuth  = webCfg ^. option #studentAPINoAuth

    studentCheckAction <- createStudentCheckAction botConfig
    jwtKey <- liftIO $ generateKey
    let jwtSettings = defaultJWTSettings jwtKey
        cookieSettings = defaultCookieSettings
    let srvCtx = cookieSettings :. jwtSettings :.
                 studentCheckAction :. studentAPINoAuth :.
                 EmptyContext

    logInfo $ "Serving Student API on "+|serverAddress|+""
    unliftIO <- askUnliftIO
    let educatorApiServer = mkMultiEducatorApiServer jwtSettings cookieSettings (convertEducatorApiHandler unliftIO)
    studentApiServer <- mkStudentApiServer (convertStudentApiHandler unliftIO) botConfig
    let witnessApiServer = if withWitnessApi
          then mkWitnessAPIServer (convertWitnessHandler unliftIO)
          else throwAll err405{ errBody = "Witness API disabled at this port" }
    let metricsEndpoint = witnessConfig ^. sub #witness . option #metricsEndpoint
    serveWeb serverAddress $
      responseTimeMetric metricsEndpoint $
      educatorCors $
      serveWithContext (Proxy @EducatorWebAPI) srvCtx $
         educatorApiServer
         :<|>
         studentApiServer
         :<|>
         witnessApiServer
