{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Necessary types and implementation for Educator authentication
module Dscp.MultiEducator.Web.Educator.Auth
       ( EducatorAuthToken (..)
       , EducatorAuthData (..)
       , MultiEducatorAuth
       , MultiEducatorPublicKey (..)
       ) where

import Crypto.Error (onCryptoFailure)
import Crypto.JOSE.Types (Base64Octets (..))
import Crypto.JWT (NumericDate (..))
import Crypto.PubKey.Ed25519 (publicKey)
import Data.Aeson (FromJSON(..), ToJSON(..), decodeStrict)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Servant.Auth.Server (FromJWT, ToJWT, AuthCheck)
import Servant.Auth.Server.Internal.Class (IsAuth(..))

import Dscp.Crypto
import Dscp.Educator.Web.Auth

import qualified Data.ByteArray as BA

---------------------------------------------------------------------------
-- Data types
---------------------------------------------------------------------------

newtype EducatorAuthData = EducatorAuthData
    { eadId :: Text
    }

deriveJSON defaultOptions ''EducatorAuthData

instance FromJWT EducatorAuthData
instance ToJWT EducatorAuthData

data EducatorAuthToken = EducatorAuthToken
    { eatData :: EducatorAuthData
    , eatExp  :: NumericDate
    }

deriveJSON defaultOptions ''EducatorAuthToken

instance FromJWT EducatorAuthToken
instance ToJWT EducatorAuthToken

---------------------------------------------------------------------------
-- Data types
---------------------------------------------------------------------------

-- | Custom authentication type for auth-servant
data MultiEducatorAuth

-- | Type that holds MultiEducator's public key
newtype MultiEducatorPublicKey = MultiEducatorPublicKey PublicKey deriving Show

instance IsAuth MultiEducatorAuth EducatorAuthToken where
    type AuthArgs MultiEducatorAuth = '[MultiEducatorPublicKey]
    runAuth _ _ = multiEducatorAuthCheck

instance IsClientAuth MultiEducatorAuth where
    data ClientAuthData MultiEducatorAuth = MultiEducatorClientAuthData SecretKey
    provideAuth req (MultiEducatorClientAuthData sk) = signRequestBasic sk req

-- | Custom JSON instance for 'MultiEducatorPublicKey':
-- they use 'Base64Octets' for encoding and decoding
instance ToJSON MultiEducatorPublicKey where
    toJSON (MultiEducatorPublicKey pk) =
        toJSON . Base64Octets . BA.convert $ unAbstractPk pk

instance FromJSON MultiEducatorPublicKey where
    parseJSON = fmap (MultiEducatorPublicKey . toDscpPubKey) . parseJSON
      where
        toDscpPubKey = onCryptoFailure (error . show) AbstractPK . toCryptoPubKey
        toCryptoPubKey (Base64Octets rpk) = publicKey rpk

---------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------

-- | Checks the signature of the JWT and returns an 'EducatorAuthToken'
multiEducatorAuthCheck :: MultiEducatorPublicKey -> AuthCheck EducatorAuthToken
multiEducatorAuthCheck (MultiEducatorPublicKey mpk) = do
    (pk, payload) <- checkJWitness
    educatorAuthToken <- maybe mempty pure $ decodeStrict payload
    -- Check expiration time
    currentTime <- liftIO getCurrentTime
    let NumericDate expirationTime = eatExp educatorAuthToken
    guard (currentTime < addUTCTime authTimeout expirationTime)
    -- Remember about timing attacks
    guard (pk `constTimeEq` mpk)
    return educatorAuthToken
