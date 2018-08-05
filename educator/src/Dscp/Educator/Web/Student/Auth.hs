{-# LANGUAGE DataKinds #-}
-- | Nessesary types and implementation for Student authenthication

module Dscp.Educator.Web.Student.Auth
       ( StudentAuth
       , WithStudent (..)
       , AuthData (..)
       , GetStudentsAction (..)
       ) where

import Crypto.JOSE.JWK (KeyMaterial (..), KeyOp (..), OKPKeyParameters (..), jwkKeyOps)
import Crypto.JWT (emptyClaimsSet, fromKeyMaterial)
import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Network.Wai (rawPathInfo)
import Servant.Auth.Server (AuthCheck (..), FromJWT, ToJWT, defaultJWTSettings, encodeJWT,
                            jwtAuthCheck)
import Servant.Auth.Server.Internal.Class (AuthArgs (..), IsAuth (..), runAuth)

import Dscp.Core (Address (..), Student)
import Dscp.Crypto (AbstractPK (..), PublicKey, hash)

---------------------------------------------------------------------------
-- Data types
---------------------------------------------------------------------------

-- | Custom authenthication type
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

instance ToJWT (WithStudent a) where
    -- `servant-auth` insists on putting a JWT of this type into cookie headers
    -- and I couldn't find a way to disable that.
    -- Since making a ToJWT instance for this type doesn't make sense, but is
    -- required by `servant-auth` we just placeholder it with an empty claims.
    -- Note: this instance should *never* be used outside of what servant-auth
    -- is using it for: setting the cookie headers.
    encodeJWT = const emptyClaimsSet

instance FromJSON AuthData
instance ToJSON AuthData
instance FromJWT AuthData
instance ToJWT AuthData

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
