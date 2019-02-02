module Dscp.Educator.Web.Student.Client.Logic
       ( StudentApiClientError
       , StudentApiClient
       , StudentApiClientM
       , StudentApiClientNoAuth
       , StudentApiClientNoAuthM
       , hoistStudentApiClient
       , createStudentApiClient
       , requestStudent
       , requestStudentNoAuth
       ) where

import Servant.Client (ClientM, client, runClientM)
import Servant.Generic (fromServant)
import Servant.Util ()

import Dscp.Core
import Dscp.Crypto
import Dscp.Educator.Web.Auth
import Dscp.Educator.Web.Student.API
import Dscp.Educator.Web.Student.Auth
import Dscp.Educator.Web.Student.Error
import Dscp.Util
import Dscp.Web

-- | Exceptions which can appear from the client.
type StudentApiClientError = ClientError StudentAPIError

-- | Hoists existing @'StudentApiClient'@ to another monad.
-- TODO: use `hoistClient` after migration to servant-0.15
hoistStudentApiClient
    :: forall n m.
       (forall a. m a -> n a)
    -> StudentApiEndpoints (AsClientT m)
    -> StudentApiEndpoints (AsClientT n)
hoistStudentApiClient nat es = StudentApiEndpoints
    { sGetCourses       = nat ... sGetCourses es
    , sGetCourse        = nat ... sGetCourse es
    , sGetAssignments   = nat ... sGetAssignments es
    , sGetAssignment    = nat ... sGetAssignment es
    , sGetSubmissions   = nat ... sGetSubmissions es
    , sAddSubmission    = nat ... sAddSubmission es
    , sGetSubmission    = nat ... sGetSubmission es
    , sDeleteSubmission = nat ... sDeleteSubmission es
    , sGetProofs        = nat ... sGetProofs es
    }

-- | Client handlers for Student API with preset authentication.
type StudentApiClientNoAuthM m = StudentApiEndpoints (AsClientT m)

type StudentApiClientNoAuth = StudentApiClientNoAuthM IO

-- | Client handlers for Student API.
-- You have to provide it with secret key of a student unless authenticaion is disabled,
-- in which case it is optional.
type StudentApiClientM m = Maybe SecretKeyData -> StudentApiClientNoAuthM m

type StudentApiClient = StudentApiClientM IO

-- | Creates a new @'StudentApiClient'@ connecting to a given @'BaseUrl'@
createStudentApiClient :: MonadIO m => BaseUrl -> m StudentApiClient
createStudentApiClient netAddr = do
    cliEnv <- buildClientEnv netAddr
    let nat :: ClientM a -> IO a
        nat act = runClientM act cliEnv
              >>= leftToThrow (servantToClientError @StudentAPIError)

    let mkCliAuth = CliAuthData . StudentClientAuthData

    let es :: Maybe SecretKey -> StudentApiEndpoints (AsClientT ClientM)
        es mSk = fromServant $ client protectedStudentAPI (fmap mkCliAuth mSk)
    return $ \mStudentSk -> hoistStudentApiClient nat (es $ fmap skSecret mStudentSk)

-- | Helper which performs a request to server.
requestStudent
    :: (StudentApiEndpoints (AsClientT m) -> a)
    -> StudentApiClientM m
    -> SecretKeyData
    -> a
requestStudent field cli sk = field $ cli (Just sk)

-- | Helper which performs a request to server with disabled authentication.
requestStudentNoAuth
    :: (StudentApiEndpoints (AsClientT m) -> a)
    -> StudentApiClientM m
    -> a
requestStudentNoAuth field cli = field $ cli Nothing
