{-# LANGUAGE TypeOperators #-}

-- | Functions to serve Student HTTP API

module Dscp.Educator.Web.Server
       ( serveStudentAPIReal
       ) where

import Data.Proxy (Proxy (..))
import Fmt ((+|), (|+))
import Loot.Log (logInfo)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant ((:<|>) (..), Context (..), Handler, Server, err401, hoistServer,
                hoistServerWithContext, serveWithContext)
import Servant.Auth.Server (AuthResult (..), CookieSettings, JWTSettings, defaultCookieSettings,
                            defaultJWTSettings, generateKey, throwAll)
import Servant.Generic (toServant)

import Dscp.Core (Student)
import Dscp.Crypto (PublicKey, keyGen, withIntSeed)
import Dscp.Educator.Config (HasEducatorConfig)
import Dscp.Educator.Launcher (EducatorRealMode, EducatorWorkMode)
import Dscp.Educator.Web.Bot (EducatorBotSwitch (..), addBotHandlers, initializeBot)
import Dscp.Educator.Web.Educator (EducatorAPI, convertEducatorApiHandler, educatorAPI,
                                   educatorApiHandlers)
import Dscp.Educator.Web.Params (EducatorWebParams (..))
import Dscp.Educator.Web.Student (GetStudentsAction (..), ProtectedStudentAPI, StudentAPI,
                                  WithStudent (..), convertStudentApiHandler, studentAPI,
                                  studentApiHandlers)
import Dscp.Web (ServerParams (..), serveWeb)

type EducatorWebAPI =
    EducatorAPI
    :<|>
    ProtectedStudentAPI

mkEducatorApiServer
    :: forall ctx m. EducatorWorkMode ctx m
    => (forall x. m x -> Handler x)
    -> Server EducatorAPI
mkEducatorApiServer nat =
    hoistServer educatorAPI nat (toServant educatorApiHandlers)

mkStudentApiServer
    :: forall ctx m. EducatorWorkMode ctx m
    => (forall x. m x -> Handler x)
    -> EducatorBotSwitch
    -> m (Server ProtectedStudentAPI)
mkStudentApiServer nat botSwitch = do
    case botSwitch of
      EducatorBotOff -> return $ addAuth (getServer . studentApiHandlers)
      EducatorBotOn params -> initializeBot params $ do
        return . addAuth $ (\student -> getServer . addBotHandlers student . studentApiHandlers $ student)
  where
    getServer handlers = hoistServerWithContext
        studentAPI
        (Proxy :: Proxy '[CookieSettings, JWTSettings, GetStudentsAction])
        nat
        (toServant handlers)
    addAuth :: (Student -> Server StudentAPI) -> Server ProtectedStudentAPI
    addAuth h = \case
        Authenticated (WithStudent student _) -> h student
        _ -> throwAll err401

serveStudentAPIReal :: HasEducatorConfig => EducatorWebParams -> EducatorRealMode ()
serveStudentAPIReal EducatorWebParams{..} = do
    let ServerParams{..} = ewpServerParams
    -- We generate a JWK here because it is required by servant-auth.
    -- In practice it is not used anywhere.
    key <- liftIO $ generateKey
    let jwtCfg = (defaultJWTSettings key)
    (tvr :: TVar [PublicKey]) <- atomically $ newTVar []
    let getStudents :: IO [PublicKey]
        getStudents = atomically . readTVar $ tvr
    let srvCtx = jwtCfg :. defaultCookieSettings :. (GetStudentsAction getStudents) :. EmptyContext
    let (_, pk)   = withIntSeed 1000 keyGen
    let (_, pk2) = withIntSeed 1001 keyGen
    liftIO . atomically $ modifyTVar' tvr ([pk, pk2] ++)


    logInfo $ "Serving Student API on "+|spAddr|+""
    eCtx <- ask
    let educatorApiServer = mkEducatorApiServer (convertEducatorApiHandler eCtx)
    studentApiServer <- mkStudentApiServer (convertStudentApiHandler eCtx) ewpBotParams
    let ourCors = cors (const $ Just $
                        simpleCorsResourcePolicy
                        { corsRequestHeaders = [hContentType] })
    serveWeb spAddr $
      ourCors $
      serveWithContext (Proxy @EducatorWebAPI) srvCtx $
         educatorApiServer
         :<|>
         studentApiServer
