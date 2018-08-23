{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Necessary types and implementation for web server authenthication
module Dscp.Educator.Web.Auth
       ( Auth'
       , AuthData (..)
       , checkAuthData
       , WithCommonAuthData (..)
       ) where

import Data.Aeson (FromJSON)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Network.Wai (rawPathInfo)
import Servant ((:>), HasServer, ServantErr (..), ServerT, err401, hoistServerWithContext, route)
import Servant.Auth.Server (AuthCheck (..), AuthResult (..), FromJWT)
import Servant.Auth.Server.Internal.Class (AreAuths (..), runAuths)
import Servant.Server.Internal.RoutingApplication (DelayedIO, addAuthCheck, delayedFailFatal,
                                                   withRequest)

---------------------------------------------------------------------------
-- Data types
---------------------------------------------------------------------------

-- | Custom authetication API type.
data Auth' auth res

-- | A type that reperesents the data that the client has sent to authenthicate
data AuthData = AuthData
    { adPath :: Text
    , adTime :: UTCTime
    }
    deriving Generic

-- | A data type that contains common authenthication data ('AuthData')
data WithCommonAuthData a = WithCommonAuthData AuthData a

---------------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------------

instance ( HasServer api ctxs
         , AreAuths '[auth] ctxs (WithCommonAuthData res)
         ) => HasServer (Auth' auth res :> api) ctxs where
    type ServerT (Auth' auth res :> api) m = res -> ServerT api m

    hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

    route _ context subserver = route (Proxy :: Proxy api) context (addAuthCheck subserver authCheck)
      where
        authMsg :: ByteString
        authMsg = "Set Authorization header with a proper JWT"
        authCheck :: DelayedIO res
        authCheck = withRequest $ \req -> do
            let authChecks = runAuths (Proxy :: Proxy '[auth]) context
            authRes <- liftIO $ runAuthCheck (authChecks >>= checkAuthData) req
            case authRes of
                (Authenticated (r :: res)) -> return r
                _ -> delayedFailFatal $ err401 { errHeaders = [("WWW-Authenticate", authMsg)] }

instance FromJSON AuthData
instance FromJWT AuthData

---------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------

checkAuthData :: WithCommonAuthData r -> AuthCheck r
checkAuthData (WithCommonAuthData AuthData {..} r) = do
    request <- ask
    -- request path verification
    guard (adPath == decodeUtf8 (rawPathInfo request))
    -- time verification
    curTime <- liftIO $ getCurrentTime
    guard (diffUTCTime curTime adTime <= 300)
    return r
