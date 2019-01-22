{-# LANGUAGE TypeInType #-}

-- | Necessary types and implementation for web server authenthication
-- TODO: move all authentication stuff not directly related to Educator
-- to a separate module.
module Dscp.Educator.Web.Auth
       ( Auth'
       , AuthData (..)
       , checkAuthBasic

       , NoAuth
       , NoAuthData
       , NoAuthContext (..)

       , makeAuthToken
       ) where

import Crypto.JOSE (Error, decodeCompact, encodeCompact)
import Data.Aeson (FromJSON (..), decodeStrict, encode, withObject, (.:))
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Kind (type (*))
import qualified Data.List as L (lookup)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.TypeLits (Symbol)
import Network.Wai (Request, rawPathInfo, requestHeaders)
import Servant ((:>), HasServer, ServantErr (..), ServerT, err401, hoistServerWithContext, route)
import Servant.Auth.Server (AuthCheck (..), AuthResult (..))
import Servant.Auth.Server.Internal.Class (AreAuths (..), IsAuth (..), runAuths)
import Servant.Server.Internal.RoutingApplication (DelayedIO, addAuthCheck, delayedFailFatal,
                                                   withRequest)
import Servant.Util (ApiCanLogArg (..), ApiHasArgClass (..))

import Dscp.Crypto

---------------------------------------------------------------------------
-- Data types
---------------------------------------------------------------------------

-- | Custom authetication API type.
data Auth' (auths :: [*]) res

-- | A type that reperesents the data that the client has sent to authenthicate
data AuthData = AuthData
    { adPath :: !Text
    , adTime :: !UTCTime
    }
    deriving Generic

deriveJSON defaultOptions ''AuthData

---------------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------------

instance ( HasServer api ctxs
         , AreAuths auths ctxs res
         ) => HasServer (Auth' auths res :> api) ctxs where
    type ServerT (Auth' auths res :> api) m = res -> ServerT api m

    hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

    route _ context subserver = route (Proxy :: Proxy api) context (addAuthCheck subserver authCheck)
      where
        authMsg :: ByteString
        authMsg = "Set Authorization header with a proper JWS signature"
        authCheck :: DelayedIO res
        authCheck = withRequest $ \req -> do
            let authChecks = runAuths (Proxy :: Proxy auths) context
            authRes <- liftIO $ runAuthCheck authChecks req
            case authRes of
                (Authenticated (r :: res)) -> return r
                _ -> delayedFailFatal $ err401 { errHeaders = [("WWW-Authenticate", authMsg)] }

instance ApiHasArgClass (Auth' auths res) where
    apiArgName = const "Authenticate"

instance ApiCanLogArg (Auth' auths res)

---------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------

-- | If request has header "Authorization: Bearer <token>", get the
-- "<token>" part.
authBearerToken :: Request -> Maybe ByteString
authBearerToken =
    L.lookup "Authorization" . requestHeaders >=>
    BS.stripPrefix "Bearer "

checkJWitness :: AuthCheck (PublicKey, ByteString)
checkJWitness = do
    request <- ask
    token <- maybe mempty pure $ authBearerToken request
    either (const mempty) pure $ do
        jWitness <- first (show @Text @Error) $
            decodeCompact $ LBS.fromStrict token
        verifyJWitness jWitness

-- | Authentication header timeout in seconds.
-- TODO: 1) make it configurable via config file
--       2) isn't 5 minutes too long?
authTimeout :: NominalDiffTime
authTimeout = 300

checkAuthData :: AuthData -> AuthCheck ()
checkAuthData AuthData {..} = do
    request <- ask
    -- request path verification
    guard (adPath == decodeUtf8 (rawPathInfo request))
    -- time verification
    curTime <- liftIO $ getCurrentTime
    guard (diffUTCTime curTime adTime <= authTimeout)

checkAuthBasic :: AuthCheck PublicKey
checkAuthBasic = do
    (pk, payload) <- checkJWitness
    authData <- maybe mempty pure $ decodeStrict payload
    checkAuthData authData
    pure pk

---------------------------------------------------------------------------
-- Disabling auth
---------------------------------------------------------------------------

-- | Do not require authentication.
-- Type parameter @ s @ identifies 'NoAuth' so that there is 1-to-1 relation
-- between 'NoAuth' and its 'NoAuthContext'.
data NoAuth (s :: Symbol)

-- You will possibly need to provide some data which is usually provided
-- by normal authentication.
type family NoAuthData (s :: Symbol) :: *

-- | 'NoAuth' settings.
data NoAuthContext s
    = NoAuthOnContext (NoAuthData s)
    | NoAuthOffContext

deriving instance Show (NoAuthData s) => Show (NoAuthContext s)
deriving instance Eq (NoAuthData s) => Eq (NoAuthContext s)

instance (d ~ NoAuthData s) => IsAuth (NoAuth s) d where
    type AuthArgs (NoAuth s) = '[NoAuthContext s]
    runAuth _ _ = \case
        NoAuthOnContext dat -> pure dat
        NoAuthOffContext -> mzero

instance FromJSON (NoAuthData s) => FromJSON (NoAuthContext s) where
    parseJSON = withObject "no-auth context" $ \o -> do
        enabled <- o .: "enabled"
        if enabled
            then NoAuthOnContext <$> (o .: "data")
            else pure NoAuthOffContext

---------------------------------------------------------------------------
-- Testing
---------------------------------------------------------------------------

-- | Make header suitable for authentication.
-- You can pass this to @curl@ as @-H "Authorization: Bearer <produced text>"@.
-- Second arguments stands for endpoint name, example:
-- @/api/educator/v1/students@.
makeAuthToken :: SecretKey -> Text -> Text
makeAuthToken secretKey endpoint =
    decodeUtf8 . encodeCompact . signJWitness secretKey $ encode authData
  where
    farFuture = posixSecondsToUTCTime 1735689600 -- 1 Jan 2025
    authData = AuthData { adPath = endpoint, adTime = farFuture }
