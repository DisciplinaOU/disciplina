{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Necessary types and implementation for Educator authenthication
module Dscp.Educator.Web.Educator.Auth
       ( EducatorPublicKeys (..)
       , EducatorAuth
       ) where

import Crypto.JOSE.JWK (KeyMaterial (..), KeyOp (..), OKPKeyParameters (..), jwkKeyOps)
import Crypto.JWT (fromKeyMaterial)
import Servant.Auth.Server (AuthCheck (..), defaultJWTSettings, jwtAuthCheck)
import Servant.Auth.Server.Internal.Class (AuthArgs (..), IsAuth (..))

import Dscp.Crypto (AbstractPK (..))
import Dscp.Educator.Launcher.Mode (EducatorNode)
import Dscp.Educator.Web.Auth (WithCommonAuthData (..))
import Dscp.Resource.Keys (KeyResources, krPublicKey)

---------------------------------------------------------------------------
-- Data types
---------------------------------------------------------------------------

-- | Custom authentication type for auth-servant
data EducatorAuth

-- | Type that holds Educator's public key
newtype EducatorPublicKeys = EducatorPublicKeys [KeyResources EducatorNode]

instance IsAuth EducatorAuth (WithCommonAuthData (KeyResources EducatorNode)) where
    type AuthArgs EducatorAuth = '[EducatorPublicKeys]
    runAuth _ _ = educatorAuthCheck

---------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------

-- | This function returns AuthCheck that checks the signature of the JWT.
educatorAuthCheck :: EducatorPublicKeys -> AuthCheck (WithCommonAuthData (KeyResources EducatorNode))
educatorAuthCheck (EducatorPublicKeys educatorKeys) = do
    --let jwk =
    --      fromKeyMaterial (OKPKeyMaterial $ Ed25519Key pub Nothing) & jwkKeyOps .~ Just [Verify]
    --authData <- jwtAuthCheck . defaultJWTSettings $ jwk
    -- TODO: this is Ctrl+V from student Auth. It is probably should be generalized in ../Auth.hs
    let pubKeyToJwk (AbstractPK pub) =
          fromKeyMaterial (OKPKeyMaterial $ Ed25519Key pub Nothing) & jwkKeyOps .~ Just [Verify]
        tryAuth key = fmap (key,) . jwtAuthCheck . defaultJWTSettings . pubKeyToJwk $ key ^. krPublicKey
    (key, authData) <- asum . map tryAuth $ educatorKeys
    return $ WithCommonAuthData authData key
