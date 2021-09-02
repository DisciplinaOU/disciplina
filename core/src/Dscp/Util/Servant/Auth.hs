{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Dscp.Util.Servant.Auth
       ( Auth'
       , AuthToken (..)

       , IsClientAuth (..)
       , ClientAuthDataFrom (..)

       , NoAuth
       , NoAuthData
       , NoAuthContext (..)
       , whenNoAuth

       , AuthTimeout (..)
       , defaultAuthTimeout

       , authBearerToken
       , checkJWitness
       , checkAuthTime
       , requestEndpoint
       , authDataToJWT
       , clientRequestEndpoint
       , toAuthTokenBearer
       ) where

import Crypto.JOSE (Error, decodeCompact, encodeCompact)
import Data.Aeson (FromJSON (..), ToJSON (..), encode, withObject, (.:))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.List as L (lookup)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.Typeable (cast)
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)
import Network.Wai (Request, rawPathInfo, requestHeaders)
import Servant ((:>), HasServer, ServantErr (..), ServerT, err401, hoistServerWithContext, route)
import Servant.Auth.Server (AuthCheck (..), AuthResult (..))
import Servant.Auth.Server.Internal.Class (AreAuths (..), IsAuth (..), runAuths)
import Servant.Client (HasClient (..))
import Servant.Client.Core (RunClient (..))
import qualified Servant.Client.Core.Internal.Request as Cli
import Servant.Server.Internal.RoutingApplication (DelayedIO, addAuthCheck, delayedFailFatal,
                                                   withRequest)
import Servant.Util (ApiCanLogArg (..), ApiHasArgClass (..))
import Time (Second, Time, minute, toNum, toUnit)

import Dscp.Crypto
import Dscp.Util
import Dscp.Util.Type
import Dscp.Util.Servant.Hoist

---------------------------------------------------------------------------
-- Data types
---------------------------------------------------------------------------

-- | Custom authetication API type.
data Auth' (auths :: [*]) res

---------------------------------------------------------------------------
-- Server instances
---------------------------------------------------------------------------

instance ( HasServer api ctx
         , AreAuths auths ctx res
         ) => HasServer (Auth' auths res :> api) ctx where
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
    apiArgName = const "authenticated"

instance ApiCanLogArg (Auth' auths res)

---------------------------------------------------------------------------
-- Client
---------------------------------------------------------------------------

-- | One piece of authentication data, e.g. @Bearer <token>@.
newtype AuthToken = AuthToken { unAuthToken :: ByteString }

-- | Provides a way to encode this type of authentication in client.
class IsClientAuth auth where
    -- | Authentication data.
    data ClientAuthData auth :: *

    -- | By the request built so far and authentication data, make authentication header.
    provideAuth :: Cli.Request -> ClientAuthData auth -> IO AuthToken

-- | Whether supplied @ClientAuthData auth@ relate to any of authentication types
-- in @auths@.
type family KnownClientAuth auth (auths :: [*]) :: Constraint where
    KnownClientAuth authData '[] =
        TypeError
        ('Text "API does not know this authentication type: " ':<>: 'ShowType authData)
    KnownClientAuth authData (auth ': auths) =
        If (auth == authData)
            (() :: Constraint)
            (KnownClientAuth authData auths)

type family AuthNotMentioned (auth :: *) (auths :: [*]) :: Constraint where
    AuthNotMentioned auth '[] = ()
    AuthNotMentioned auth (auth ': auths) = TypeError
        ('Text "Duplicated auth type " ':<>: 'ShowType auth)
    AuthNotMentioned auth (auth0 ': auths) =
        AuthNotMentioned auth auths

-- | Whether all types in @auths@ are different.
type family AllUniqueAuths (auths :: [*]) :: Constraint where
    AllUniqueAuths '[] = ()
    AllUniqueAuths (auth ': auths) = (AuthNotMentioned auth auths, AllUniqueAuths auths)

-- | Implementation of 'IsClientAuth' for some @auth@.
data SomeAuthProvider =
    forall (auth :: *). Typeable auth =>
    SomeAuthProvider (Cli.Request -> ClientAuthData auth -> IO AuthToken)

-- | Multi-version of 'IsClientAuth'.
class AreClientAuths (auths :: [*]) where
    provideAuths :: [SomeAuthProvider]

instance AreClientAuths '[] where
    provideAuths = []

instance (IsClientAuth auth, Typeable auth, AreClientAuths auths) =>
         AreClientAuths (auth ': auths) where
    provideAuths = SomeAuthProvider (provideAuth @auth) : provideAuths @auths

-- | Some 'ClientAuthData' complying one of @auths@.
data ClientAuthDataFrom (auths :: [*]) =
    forall auth. (KnownClientAuth auth auths, Typeable auth) =>
    CliAuthData (ClientAuthData auth)

-- | Form an authentication header from provided authentication data.
encodeAuthData
    :: forall auths.
       (Typeable auths, AreClientAuths auths, AllUniqueAuths auths)
    => Cli.Request -> ClientAuthDataFrom auths -> IO AuthToken
encodeAuthData req (CliAuthData authData) =
    -- The list constructed in the following do-block is not empty due to
    -- invariants of 'ClientAuthDataFrom'.
    -- Also, it contains no more than one element since we forced all @auths@ to be
    -- unique, thus all corresponding 'ClientAuthData's are unique as well.
    oneOrError $ do
        SomeAuthProvider authProvider <- provideAuths @auths
        Just matchingAuthData <- pure (cast authData)
        pure $ authProvider req matchingAuthData

-- | Supposedly a wrapper over 'ClientM', it carries 'MakeAuthHeaders' till the moment
-- a request is fully built and inserts authentication header upon submission.
newtype AuthClientM auths m a = AuthClientM
    { unAuthClientM :: ReaderT (Maybe (ClientAuthDataFrom auths)) m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

runAuthClientM :: Maybe (ClientAuthDataFrom auths) -> AuthClientM auths m a -> m a
runAuthClientM ctx action = runReaderT (unAuthClientM action) ctx

-- | Attaches authentication header to given request.
runAuthRequest
    :: (MonadIO m, AreClientAuths auths, AllUniqueAuths auths, Typeable auths)
    => (Cli.Request -> m a) -> Cli.Request -> AuthClientM auths m a
runAuthRequest runReq req =
    AuthClientM . ReaderT $ \mAuthData -> do
        mAuthToken <- liftIO $ mapM (encodeAuthData req) mAuthData
        let req' = maybe id addAuthToken mAuthToken req
        runReq req'
  where
    addAuthToken = Cli.addHeader "Authorization" . decodeUtf8 @Text . unAuthToken

instance ( MonadIO m, RunClient m
         , AreClientAuths auths, AllUniqueAuths auths, Typeable auths
         ) =>
         RunClient (AuthClientM auths m) where
    runRequest = runAuthRequest runRequest
    streamingRequest = runAuthRequest streamingRequest
    throwServantError = AuthClientM . lift . throwServantError @m
    catchServantError action handler =
        AuthClientM . ReaderT $ \ctx ->
            catchServantError @m (runAuthClientM ctx action)
                                 (runAuthClientM ctx . handler)

-- | Generally, on client side you are allowed to optionally provide authentication data
-- which will appear in an authentication header.
instance ( HasClient (AuthClientM auths m) api
         , RunClient m
         , CanHoistClient (AuthClientM auths m) api
         , Typeable auths, AreClientAuths auths, AllUniqueAuths auths
         ) =>
         HasClient m (Auth' auths res :> api) where

    type Client m (Auth' auths res :> api) =
        Maybe (ClientAuthDataFrom auths) -> Client m api

    clientWithRoute _ _ semiBuiltRequest mAuthData =
        hoistClientMonad (Proxy @(AuthClientM auths m)) (Proxy @api) (runAuthClientM mAuthData) $
        clientWithRoute (Proxy @(AuthClientM auths m)) (Proxy @api) semiBuiltRequest

---------------------------------------------------------------------------
-- Disabling auth
---------------------------------------------------------------------------

-- | Do not require authentication.
-- Type parameter @ s @ makes 'NoAuth's distinguishable so that 1-to-1 relation
-- between 'NoAuth' and its 'NoAuthContext' could exist.
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

instance IsClientAuth (NoAuth s) where
    data ClientAuthData (NoAuth s)
    provideAuth _ = \case

instance FromJSON (NoAuthData s) => FromJSON (NoAuthContext s) where
    parseJSON = withObject "no-auth context" $ \o -> do
        enabled <- o .: "enabled"
        if enabled
            then NoAuthOnContext <$> (o .: "data")
            else pure NoAuthOffContext

whenNoAuth :: Monad m => NoAuthContext s -> (NoAuthData s -> m ()) -> m ()
whenNoAuth ctx action = case ctx of
    NoAuthOnContext c -> action c
    NoAuthOffContext -> pass

---------------------------------------------------------------------------
-- Helper
---------------------------------------------------------------------------

-- | Useful type to check the timeout of an auth request
newtype AuthTimeout = AuthTimeout { unAuthTimeout :: Time Second }
    deriving (Show, Eq)

instance ToJSON AuthTimeout where
    toJSON = toJSON . unAuthTimeout

instance FromJSON AuthTimeout where
    parseJSON = fmap AuthTimeout . parseJSON

defaultAuthTimeout :: AuthTimeout
defaultAuthTimeout = AuthTimeout . toUnit @Second $ minute 5

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

checkAuthTime
    :: UTCTime           -- ^ Time to check
    -> Maybe AuthTimeout -- ^ Max allowed difference (if any) from current time
    -> AuthCheck ()
checkAuthTime adTime mAuthTimeout = do
    curTime <- liftIO $ getCurrentTime
    let authTimeout = maybe 0 (toNum @Second . unAuthTimeout) mAuthTimeout
    guard (diffUTCTime curTime adTime <= authTimeout)

-- | Get endpoint name from request.
requestEndpoint :: Request -> Text
requestEndpoint = decodeUtf8 . rawPathInfo

-- | Sign authentiation data and produce JWT token.
-- Second argument stands for endpoint name, example:
-- @/api/educator/v1/students@.
authDataToJWT :: (ToJSON authData) => SecretKey -> authData -> ByteString
authDataToJWT sk = LBS.toStrict . encodeCompact . signJWitness sk . encode

-- | Extract endpoint name from built request.
clientRequestEndpoint :: Cli.Request -> Text
clientRequestEndpoint = decodeUtf8 . toLazyByteString . Cli.requestPath

toAuthTokenBearer :: ByteString -> AuthToken
toAuthTokenBearer = AuthToken . ("Bearer " <>)
