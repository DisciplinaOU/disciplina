{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

-- | Functions to serve Student HTTP API

module Dscp.Educator.Web.Server
       ( serveEducatorAPIsReal
       ) where

import Data.Proxy (Proxy (..))
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
import Dscp.DB.SQLite (SQLiteDB, existsStudent, invoke)
import Dscp.Educator.Launcher.Mode (CombinedWorkMode, EducatorNode, EducatorWorkMode)
import Dscp.Educator.Web.Auth
import Dscp.Educator.Web.Bot (EducatorBotSwitch (..), addBotHandlers, initializeBot)
import Dscp.Educator.Web.Educator (EducatorPublicKey (..), ProtectedEducatorAPI,
                                   convertEducatorApiHandler, educatorApiHandlers,
                                   protectedEducatorAPI)
import Dscp.Educator.Web.Params (EducatorWebParams (..))
import Dscp.Educator.Web.Student (ProtectedStudentAPI, StudentCheckAction (..),
                                  convertStudentApiHandler, studentAPI, studentApiHandlers)
import Dscp.Resource.Keys (KeyResources, krPublicKey)
import Dscp.Web (ServerParams (..), serveWeb)
import Dscp.Web.Metrics (responseTimeMetric)
import Dscp.Witness.Config
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
    -> EducatorBotSwitch
    -> m (Server ProtectedStudentAPI)
mkStudentApiServer nat botSwitch = do
    case botSwitch of
      EducatorBotOff -> return $ getServer . studentApiHandlers
      EducatorBotOn params -> initializeBot params $ do
        return $ (\student -> getServer . addBotHandlers student . studentApiHandlers $ student)
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
    => EducatorBotSwitch
    -> m StudentCheckAction
createStudentCheckAction (EducatorBotOn _) =
    return . StudentCheckAction . const $ pure True
createStudentCheckAction EducatorBotOff = do
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

serveEducatorAPIsReal :: CombinedWorkMode ctx m => Bool -> EducatorWebParams -> m ()
serveEducatorAPIsReal withWitnessApi EducatorWebParams{..} = do
    let ServerParams{..} = ewpServerParams
    educatorKeyResources <- view (lensOf @(KeyResources EducatorNode))
    studentCheckAction <- createStudentCheckAction ewpBotParams
    let educatorPublicKey = EducatorPublicKey $ educatorKeyResources ^. krPublicKey
    let srvCtx = educatorPublicKey :. ewpEducatorAPINoAuth :.
                 studentCheckAction :. ewpStudentAPINoAuth :.
                 EmptyContext

    logInfo $ "Serving Student API on "+|spAddr|+""
    unliftIO <- askUnliftIO
    let educatorApiServer = mkEducatorApiServer (convertEducatorApiHandler unliftIO)
    studentApiServer <- mkStudentApiServer (convertStudentApiHandler unliftIO) ewpBotParams
    let witnessApiServer = if withWitnessApi
          then mkWitnessAPIServer (convertWitnessHandler unliftIO)
          else throwAll err405{ errBody = "Witness API disabled at this port" }
    let metricsEndpoint = witnessConfig ^. sub #witness . option #metricsEndpoint
    serveWeb spAddr $
      responseTimeMetric metricsEndpoint $
      educatorCors $
      serveWithContext (Proxy @EducatorWebAPI) srvCtx $
         educatorApiServer
         :<|>
         studentApiServer
         :<|>
         witnessApiServer
