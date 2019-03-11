module Dscp.MultiEducator.Web.Educator.Client
    ( MultiEducatorApiClientM
    , MultiEducatorApiClient
    ) where

import Servant.Client (ClientM, client, runClientM)
import Servant.Generic (fromServant)

import Dscp.Educator.Web.Educator.API
import Dscp.Educator.Web.Educator.Client
import Dscp.Educator.Web.Educator.Error
import Dscp.Util
import Dscp.Web

-- | Client handlers for Educator API of multi-educator.
type MultiEducatorApiClientM m = Int -> EducatorApiClientNoAuthM m

type MultiEducatorApiClient = MultiEducatorApiClientM IO

createMultiEducatorApiClient :: MonadIO m => BaseUrl -> m MultiEducatorApiClient
createMultiEducatorApiClient netAddr = do
    cliEnv <- buildClientEnv netAddr
    let nat :: ClientM a -> IO a
        nat act = runClientM act cliEnv
              >>= leftToThrow (servantToClientError @EducatorAPIError)

    let mkCliAuth = CliAuthData . StudentClientAuthData . skSecret

    let es :: Int -> EducatorApiEndpoints (AsClientT ClientM)
        es mSk = fromServant $ client protectedEducatorAPI (fmap mkCliAuth mSk)
    return $ \mEducatorSk -> hoistEducatorApiClient nat (es mEducatorSk)
