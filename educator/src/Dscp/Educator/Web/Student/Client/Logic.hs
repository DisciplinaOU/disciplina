module Dscp.Educator.Web.Student.Client.Logic
       ( StudentApiClientError
       , StudentApiClient
       , StudentApiClientM
       , StudentApiClientNoAuth
       , StudentApiClientNoAuthM
       , hoistStudentApiClient
       , createStudentApiClient
       ) where

import Servant.Client (ClientM, client, runClientM)
import Servant.Generic (fromServant)
import Servant.Util ()

import Dscp.Crypto
import Dscp.Educator.Web.Auth
import Dscp.Educator.Web.Student.API
import Dscp.Educator.Web.Student.Auth
import Dscp.Educator.Web.Student.Client.Error
import Dscp.Util
import Dscp.Web

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
type StudentApiClientM m = Maybe SecretKey -> StudentApiClientNoAuthM m

type StudentApiClient = StudentApiClientM IO

-- | Creates a new @'StudentApiClient'@ connecting to a given @'BaseUrl'@
createStudentApiClient :: MonadIO m => BaseUrl -> m StudentApiClient
createStudentApiClient netAddr = do
    cliEnv <- buildClientEnv netAddr
    let nat :: ClientM a -> IO a
        nat act = runClientM act cliEnv >>= leftToThrow servantToStudentApiError

    let mkCliAuth = maybe [] (one . CliAuthData . StudentClientAuthData)

    let es :: Maybe SecretKey -> StudentApiEndpoints (AsClientT ClientM)
        es mSk = fromServant $ client protectedStudentAPI (mkCliAuth mSk)
    return $ \mStudentSk -> hoistStudentApiClient nat (es mStudentSk)
