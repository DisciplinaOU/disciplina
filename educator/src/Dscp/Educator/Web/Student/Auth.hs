{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
-- | Nessesary types and implementation for Student authenthication

module Dscp.Educator.Web.Student.Auth
       ( GetStudentsAction (..)
       , Auth'
       ) where

import Crypto.JOSE.JWK (KeyMaterial (..), KeyOp (..), OKPKeyParameters (..), jwkKeyOps)
import Crypto.JWT (fromKeyMaterial)
import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Network.Wai (rawPathInfo)
import Servant ((:>), HasContextEntry, HasServer, ServantErr (..), ServerT, err401,
                hoistServerWithContext, route)
import Servant.Auth.Server (AuthCheck (..), AuthResult (..), FromJWT, defaultJWTSettings,
                            jwtAuthCheck)
import Servant.Auth.Server.Internal.Class (AuthArgs (..), IsAuth (..), runAuth, runAuths)
import Servant.Server.Internal.RoutingApplication (DelayedIO, addAuthCheck, delayedFailFatal,
                                                   withRequest)

import Dscp.Core (Address (..), Student)
import Dscp.Crypto (AbstractPK (..), PublicKey, hash)

---------------------------------------------------------------------------
-- Data types
---------------------------------------------------------------------------

-- | Custom authetication API type.
data Auth'

-- | Custom authentication type for auth-servant
data StudentAuth

-- | A type that has a Student attached to it
data WithStudent a = WithStudent Student a

-- | A type that the student has sent to authenthicate
data AuthData = AuthData
    { adPath :: Text
    , adTime :: UTCTime
    }
    deriving Generic

-- | Action to get students' public keys
newtype GetStudentsAction = GetStudentsAction (IO [PublicKey])

---------------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------------

instance ( HasServer api ctxs
         , HasContextEntry ctxs GetStudentsAction
         ) => HasServer (Auth' :> api) ctxs where
    type ServerT (Auth' :> api) m = Student -> ServerT api m

    hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

    route _ context subserver = route (Proxy :: Proxy api) context (addAuthCheck subserver authCheck)
      where
        authMsg :: ByteString
        authMsg = "Set Authorization header with a proper JWT"
        authCheck :: DelayedIO Student
        authCheck = withRequest $ \req -> do
            authRes <- liftIO $ runAuthCheck (runAuths (Proxy :: Proxy '[StudentAuth]) context) req
            case authRes of
                (Authenticated (WithStudent stud (AuthData _ _))) -> return stud
                _ -> delayedFailFatal $ err401 { errHeaders = [("WWW-Authenticate", authMsg)] }

instance FromJSON AuthData
instance ToJSON AuthData
instance FromJWT AuthData

instance IsAuth StudentAuth (WithStudent AuthData) where
    type AuthArgs StudentAuth = '[GetStudentsAction]
    runAuth _ _ = studentAuthCheck

---------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------

-- | This function returns AuthCheck that checks the signature of the JWT.
studentAuthCheck :: GetStudentsAction -> AuthCheck (WithStudent AuthData)
studentAuthCheck (GetStudentsAction getStudents) = do
    students <- liftIO $ getStudents
    request <- ask
    let pubKeyToJwk (AbstractPK pub) =
          fromKeyMaterial (OKPKeyMaterial $ Ed25519Key pub Nothing) & jwkKeyOps .~ Just [Verify]
    let tryAuth :: PublicKey -> AuthCheck (PublicKey, AuthData)
        tryAuth pub = fmap (pub,) . jwtAuthCheck . defaultJWTSettings . pubKeyToJwk $ pub
    (publicKey, authData) <- asum . map tryAuth $ students
    -- request path verification
    when (adPath authData /= decodeUtf8 (rawPathInfo request)) mzero
    -- time verification
    curTime <- liftIO $ getCurrentTime
    when (diffUTCTime curTime (adTime authData) > 300) mzero
    return (WithStudent (Address $ hash publicKey) authData)
