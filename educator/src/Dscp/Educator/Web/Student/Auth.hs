{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Necessary types and implementation for Student authenthication
module Dscp.Educator.Web.Student.Auth
       ( GetStudentsAction (..)
       , StudentAuth
       ) where

import Crypto.JOSE.JWK (KeyMaterial (..), KeyOp (..), OKPKeyParameters (..), jwkKeyOps)
import Crypto.JWT (fromKeyMaterial)
import Servant.Auth.Server (AuthCheck (..), defaultJWTSettings, jwtAuthCheck)
import Servant.Auth.Server.Internal.Class (AuthArgs (..), IsAuth (..))

import Dscp.Core (Student, mkAddr)
import Dscp.Crypto (AbstractPK (..), PublicKey)
import Dscp.Educator.Web.Auth (NoAuthData, checkAuthData)

---------------------------------------------------------------------------
-- Data types
---------------------------------------------------------------------------

-- | Custom authentication type for auth-servant
data StudentAuth

-- | Action to get students' public keys
newtype GetStudentsAction = GetStudentsAction (IO [PublicKey])

instance IsAuth StudentAuth Student where
    type AuthArgs StudentAuth = '[GetStudentsAction]
    runAuth _ _ = studentAuthCheck

---------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------

-- | This function returns AuthCheck that checks the signature of the JWT.
studentAuthCheck :: GetStudentsAction -> AuthCheck Student
studentAuthCheck (GetStudentsAction getStudents) = do
    students <- liftIO $ getStudents
    let pubKeyToJwk (AbstractPK pub) =
          fromKeyMaterial (OKPKeyMaterial $ Ed25519Key pub Nothing) & jwkKeyOps .~ Just [Verify]
        tryAuth pub = fmap (pub,) . jwtAuthCheck . defaultJWTSettings . pubKeyToJwk $ pub
    (publicKey, authData) <- asum . map tryAuth $ students
    checkAuthData authData
    return $ mkAddr publicKey

---------------------------------------------------------------------------
-- NoAuth
---------------------------------------------------------------------------

type instance NoAuthData "student" = Student
