module Dscp.Educator.Web.Student.Client.Logic
       ( StudentApiClient
       , hoistStudentApiClient
       , createStudentApiClient
       ) where

import Servant.Client (ClientM, client, runClientM)
import Servant.Generic (fromServant)
import Servant.Util ()

import Dscp.Educator.Web.Student.API
import Dscp.Educator.Web.Student.Client.Error
import Dscp.Util
import Dscp.Web

type StudentApiClient = StudentApiEndpoints (AsClientT IO)

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

-- | Creates a new @'StudentApiClient'@ connecting to a given @'BaseUrl'@
createStudentApiClient :: MonadIO m => BaseUrl -> m StudentApiClient
createStudentApiClient netAddr = do
    cliEnv <- buildClientEnv netAddr
    let nat :: ClientM a -> IO a
        nat act = runClientM act cliEnv >>= leftToThrow servantToStudentApiError

    let es :: StudentApiEndpoints (AsClientT ClientM)
        es = fromServant $ client studentAPI
    return $ hoistStudentApiClient nat es
