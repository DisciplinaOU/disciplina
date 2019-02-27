module Test.Dscp.Educator.Web.Auth
    ( spec_Educator_api_authentication
    ) where

import Data.Default (Default (..))
import Network.HTTP.Types (statusCode)
import Servant ((:>), Context (..), Handler, HasServer, Server)
import Servant.Client (GenResponse (..), ServantError (..))
import Servant.Generic (AsServerT, fromServant, toServant)
import Servant.Mock (HasMock (..), mock)
import Servant.QuickCheck (withServantServerAndContext)
import Servant.Util (ErrorResponses, PaginationParams, SortingParams, Tag)

import Dscp.Core
import Dscp.Crypto
import Dscp.Educator.Web.Student
import Dscp.Util.Test
import Dscp.Util.Servant.Auth
import Dscp.Web.Class

import Test.Dscp.Educator.Web.Instances ()

{- We will test all auth on Student API -}

instance (HasMock subApi ctx, HasServer (SortingParams params :> subApi) ctx) =>
         HasMock (SortingParams params :> subApi) ctx where
    mock _ pc _ = mock (Proxy @subApi) pc

instance (HasMock subApi ctx, HasServer (PaginationParams :> subApi) ctx) =>
         HasMock (PaginationParams :> subApi) ctx where
    mock _ pc _ = mock (Proxy @subApi) pc

instance (HasMock subApi ctx, HasServer (Tag name :> subApi) ctx) =>
         HasMock (Tag name :> subApi) ctx where
    mock _ pc = mock (Proxy @subApi) pc

instance (HasMock subApi ctx, HasServer (ErrorResponses err :> subApi) ctx) =>
         HasMock (ErrorResponses err :> subApi) ctx where
    mock _ pc = mock (Proxy @subApi) pc


requesterSK :: SecretKeyData
requesterSK = detGen 12543 arbitrary

throws401 :: MonadCatch m => m a -> m Property
throws401 = throwsMatching @StudentApiClientError $ \case
    SomeClientError (FailureResponse resp) -> statusCode (responseStatusCode resp) == 401
    _ -> False

serverProp :: Testable prop => IO prop -> Property
serverProp = once . ioProperty

mockedApiHandlers :: Server ProtectedStudentAPI
mockedApiHandlers _student =
    mock (Proxy @RawStudentAPI) (Proxy @[StudentCheckAction, NoAuthContext "student"])

data TestServerConfig = TestServerConfig
    { tscNoAuth         :: Bool
    , tscServerModifier :: Server ProtectedStudentAPI -> Server ProtectedStudentAPI
    }

instance Default TestServerConfig where
    def = TestServerConfig{ tscNoAuth = False, tscServerModifier = id }

withServerClient :: TestServerConfig -> (StudentApiClient -> IO r) -> IO r
withServerClient config action =
    withServantServerAndContext (Proxy @ProtectedStudentAPI) ctx (pure handlers) $
    \baseUrl -> action =<< createStudentApiClient baseUrl
  where
    handlers = tscServerModifier config mockedApiHandlers
    noAuthCtx
        | tscNoAuth config = NoAuthOnContext noAuthStudent
        | otherwise = NoAuthOffContext :: NoAuthContext "student"
    noAuthStudent = detGen 5643 arbitrary
    studentCheckAction = StudentCheckAction (\_ -> return True)
    ctx = studentCheckAction :. noAuthCtx :. defaultAuthTimeout :. EmptyContext

-- TODO: Maybe add a "ping" endpoint instead?
doTrialRequest :: StudentApiClient -> Maybe SecretKeyData -> IO ()
doTrialRequest cli sk = void $ sGetCourses (cli sk) Nothing False def def

modifyTrialEndpoint
    :: (forall m a. MonadIO m => Student -> m a -> m a)
    -> Server ProtectedStudentAPI
    -> Server ProtectedStudentAPI
modifyTrialEndpoint modifyEndpoint server student =
    toServant @(StudentApiEndpoints (AsServerT Handler)) $
        let gserver = fromServant @(StudentApiEndpoints (AsServerT Handler)) (server student)
        in gserver{ sGetCourses = modifyEndpoint student ... sGetCourses gserver }

spec_Educator_api_authentication :: Spec
spec_Educator_api_authentication = do
    it "Simple endpoint works" . serverProp $
        withServerClient def $ \cli ->
            doTrialRequest cli (Just requesterSK)

    it "Fails without authentication data" . serverProp $
        withServerClient def $ \cli ->
            throws401 $ doTrialRequest cli Nothing

    describe "Auth works for different API parts" $ do
        it "Capture" . serverProp $
            withServerClient def $ \cli ->
                void $ requestStudent sGetCourse cli requesterSK courseEx

        it "DELETE method" . serverProp $
            withServerClient def $ \cli ->
                requestStudent sDeleteSubmission cli requesterSK (hash submissionEx)

        it "Request body + POST" . serverProp $
            void $ withServerClient def $ \cli ->
                 requestStudent sAddSubmission cli requesterSK
                                (signedSubmissionToRequest signedSubmissionEx)

    describe "Auth works properly in presence of no-auth" $ do
        let noAuthConfig = def{ tscNoAuth = True }

        it "No-auth makes authentication optional" . serverProp $
            withServerClient noAuthConfig $ \cli ->
                doTrialRequest cli Nothing

        it "No-auth allows actual authentication" . serverProp $ do
            ref <- newIORef Nothing
            let config = noAuthConfig
                    { tscServerModifier =
                        modifyTrialEndpoint $
                            \student -> (writeIORef ref (Just student) >>)
                    }
            withServerClient config $ \cli ->
                doTrialRequest cli (Just requesterSK)

            mRequester <- readIORef ref
            return (mRequester === Just (skAddress requesterSK))
