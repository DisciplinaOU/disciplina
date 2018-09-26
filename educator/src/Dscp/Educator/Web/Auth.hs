{-# LANGUAGE TypeInType #-}

-- | Necessary types and implementation for web server authenthication
module Dscp.Educator.Web.Auth
       ( Auth'
       , AuthData (..)
       , checkAuthData

       , NoAuth
       , NoAuthData
       , NoAuthContext (..)

       , makeAuthToken
       ) where

import Crypto.JOSE.JWK ()
import Crypto.JWT (JWTError, KeyMaterial (OKPKeyMaterial), KeyOp (Sign),
                   OKPKeyParameters (Ed25519Key), addClaim, bestJWSAlg, emptyClaimsSet,
                   encodeCompact, fromKeyMaterial, jwkKeyOps, newJWSHeader, signClaims)
import Data.Aeson (FromJSON (..), Value (..), object, withObject, (.:), (.=))
import Data.Kind (type (*))
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.TypeLits (Symbol)
import Network.Wai (rawPathInfo)
import Servant ((:>), HasServer, ServantErr (..), ServerT, err401, hoistServerWithContext, route)
import Servant.Auth.Server (AuthCheck (..), AuthResult (..), FromJWT)
import Servant.Auth.Server.Internal.Class (AreAuths (..), IsAuth (..), runAuths)
import Servant.Server.Internal.RoutingApplication (DelayedIO, addAuthCheck, delayedFailFatal,
                                                   withRequest)

import Dscp.Crypto
import Dscp.Util

---------------------------------------------------------------------------
-- Data types
---------------------------------------------------------------------------

-- | Custom authetication API type.
data Auth' (auths :: [*]) res

-- | A type that reperesents the data that the client has sent to authenthicate
data AuthData = AuthData
    { adPath :: Text
    , adTime :: UTCTime
    }
    deriving Generic

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
        authMsg = "Set Authorization header with a proper JWT"
        authCheck :: DelayedIO res
        authCheck = withRequest $ \req -> do
            let authChecks = runAuths (Proxy :: Proxy auths) context
            authRes <- liftIO $ runAuthCheck authChecks req
            case authRes of
                (Authenticated (r :: res)) -> return r
                _ -> delayedFailFatal $ err401 { errHeaders = [("WWW-Authenticate", authMsg)] }

instance FromJSON AuthData
instance FromJWT AuthData

---------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------

checkAuthData :: AuthData -> AuthCheck ()
checkAuthData AuthData {..} = do
    request <- ask
    -- request path verification
    guard (adPath == decodeUtf8 (rawPathInfo request))
    -- time verification
    curTime <- liftIO $ getCurrentTime
    guard (diffUTCTime curTime adTime <= 300)

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
makeAuthToken secretKey endpoint = unsafePerformIO $ do
    eSignedJWT <- runExceptT $ do
        alg <- bestJWSAlg jwkSk
        signClaims jwkSk (newJWSHeader ((), alg)) claims
    return $
        decodeUtf8 . encodeCompact $
        leftToPanic $ first (show @Text @JWTError) eSignedJWT
  where
    (AbstractSK sk, AbstractPK pk) = (secretKey, toPublic secretKey)
    jwkSk = fromKeyMaterial (OKPKeyMaterial $ Ed25519Key pk $ Just sk)
          & jwkKeyOps .~ Just [Sign]

    claims = emptyClaimsSet & addClaim "dat" claim
    claim = object
        [ "adTime" .= String "2025-08-10T13:15:40.461998136Z"
        , "adPath" .= endpoint
        ]
