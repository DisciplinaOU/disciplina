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

import Dscp.Crypto (PublicKey, keyGen, withIntSeed)
import Dscp.Educator.Launcher.Mode (CombinedWorkMode, EducatorNode, EducatorWorkMode)
import Dscp.Educator.Web.Bot (EducatorBotSwitch (..), addBotHandlers, initializeBot)
import Dscp.Educator.Web.Educator (EducatorPublicKeys (..), ProtectedEducatorAPI,
                                   convertEducatorApiHandler, educatorApiHandlers,
                                   protectedEducatorAPI)
import Dscp.Educator.Web.Params (EducatorWebParams (..))
import Dscp.Educator.Web.Student (GetStudentsAction (..), ProtectedStudentAPI,
                                  convertStudentApiHandler, studentAPI, studentApiHandlers)
import Dscp.Resource.Keys (KeyResources, krPublicKey)
import Dscp.Web (ServerParams (..), serveWeb)
import Dscp.Web.Metrics (responseTimeMetric)
import Dscp.Witness.Config
import Dscp.Witness.Web

import Data.Reflection

import Loot.Config (option, sub)

import Dscp.Educator.Config

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
        (Proxy :: Proxy '[EducatorPublicKeys])
        nat
        (\key -> toServant (educatorApiHandlers key))

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
        (Proxy :: Proxy '[GetStudentsAction])
        nat
        (toServant handlers)

-- This is a temporary function that provides a dummy GetStudentAction
createGetStudentsAction :: IO GetStudentsAction
createGetStudentsAction = do
    (tvr :: TVar [PublicKey]) <- atomically $ newTVar []
    let addKeyWithSeed n =
            let pk = snd $ withIntSeed n keyGen
            in atomically $ modifyTVar' tvr (pk:)
    traverse_ addKeyWithSeed [1000..1100]
    return . GetStudentsAction $ atomically . readTVar $ tvr

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
    educatorKeyResources <- view (lensOf @[KeyResources EducatorNode])
    getStudents <- liftIO $ createGetStudentsAction
    let educatorPublicKey = EducatorPublicKeys educatorKeyResources
    let srvCtx = educatorPublicKey :. getStudents :. EmptyContext

    let
      educatorCfg = given :: EducatorConfigRec
      test = educatorCfg ^. sub #educator . option #keys

    liftIO $ print (length educatorKeyResources)

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
