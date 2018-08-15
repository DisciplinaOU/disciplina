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
import Servant ((:<|>) (..), Context (..), Handler, Server, hoistServer,
                hoistServerWithContext, serveWithContext)
import Servant.Generic (toServant)

import Dscp.Crypto (PublicKey, keyGen, withIntSeed)
import Dscp.Educator.Config (HasEducatorConfig)
import Dscp.Educator.Launcher (EducatorRealMode, EducatorWorkMode)
import Dscp.Educator.Web.Bot (EducatorBotSwitch (..), addBotHandlers, initializeBot)
import Dscp.Educator.Web.Educator (EducatorAPI, convertEducatorApiHandler, educatorAPI,
                                   educatorApiHandlers)
import Dscp.Educator.Web.Params (EducatorWebParams (..))
import Dscp.Educator.Web.Student (GetStudentsAction (..), ProtectedStudentAPI,
                                  convertStudentApiHandler, studentAPI,
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

serveStudentAPIReal :: HasEducatorConfig => EducatorWebParams -> EducatorRealMode ()
serveStudentAPIReal EducatorWebParams{..} = do
    let ServerParams{..} = ewpServerParams
    getStudents <- liftIO $ createGetStudentsAction
    let srvCtx = getStudents :. EmptyContext

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
