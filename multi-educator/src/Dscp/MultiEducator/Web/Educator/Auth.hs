{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Necessary types and implementation for Educator authentication
module Dscp.MultiEducator.Web.Educator.Auth
       ( EducatorAuthToken (..)
       , EducatorAuthData (..)
       , EducatorAuthLogin (..)
       , ealId
       , educatorAuthLoginSimple
       , MultiEducatorAuth
       , MultiEducatorPublicKey (..)
       ) where

import Crypto.Error (onCryptoFailure)
import Crypto.JOSE.Types (Base64Octets (..))
import Crypto.JWT (NumericDate (..))
import Crypto.PubKey.Ed25519 (publicKey)
import Data.Aeson (FromJSON (..), ToJSON (..), decodeStrict)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Fmt (build, (+||), (||+))
import Servant.Auth.Server (AuthCheck, FromJWT, ToJWT)
import Servant.Auth.Server.Internal.Class (IsAuth (..))
import System.FilePath.Posix ((</>))

import Dscp.Crypto
import Dscp.Educator.Web.Auth
import Dscp.MultiEducator.Types
import Dscp.Util.FileEmbed
import Dscp.Util.Servant.Auth

import qualified Data.ByteArray as BA

---------------------------------------------------------------------------
-- Data types
---------------------------------------------------------------------------

newtype EducatorAuthData = EducatorAuthData
    { eadId :: EducatorUUID
    } deriving (Show, Eq)

deriveJSON defaultOptions ''EducatorAuthData

instance FromJWT EducatorAuthData
instance ToJWT EducatorAuthData

instance Buildable EducatorAuthData where
    build (EducatorAuthData eId) = build eId

data EducatorAuthToken = EducatorAuthToken
    { eatData :: EducatorAuthData
    , eatExp  :: NumericDate
    }

deriveJSON defaultOptions ''EducatorAuthToken

instance FromJWT EducatorAuthToken
instance ToJWT EducatorAuthToken

data EducatorAuthLogin = EducatorAuthLogin
    { ealData  :: EducatorAuthData
    , ealToken :: ByteString
    } deriving (Show, Eq)

ealId :: EducatorAuthLogin -> EducatorUUID
ealId = eadId . ealData

instance ToJSON EducatorAuthLogin where
    toJSON (EducatorAuthLogin {..}) = toJSON ealData

instance FromJSON EducatorAuthLogin where
    parseJSON = fmap (\ealData -> EducatorAuthLogin {..}) . parseJSON
      where ealToken = mempty

instance Buildable EducatorAuthLogin where
    build (EducatorAuthLogin {..}) = build ealData <> "("+||ealToken||+")"

---------------------------------------------------------------------------
-- Data types
---------------------------------------------------------------------------

-- | Custom authentication type for auth-servant
data MultiEducatorAuth

-- | Type that holds MultiEducator's public key
newtype MultiEducatorPublicKey = MultiEducatorPublicKey PublicKey
    deriving (Show, Eq)

instance IsAuth MultiEducatorAuth EducatorAuthLogin where
    type AuthArgs MultiEducatorAuth = '[MultiEducatorPublicKey]
    runAuth _ _ = multiEducatorAuthCheck

-- We cannot implement 'IsClientAuth' since for that we would need
-- multieducator AAA secret key.

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

-- | Checks the signature of the JWT and returns an 'EducatorAuthLogin'
multiEducatorAuthCheck :: MultiEducatorPublicKey -> AuthCheck EducatorAuthLogin
multiEducatorAuthCheck (MultiEducatorPublicKey mpk) = do
    request <- ask
    ealToken <- maybe mempty pure $ authBearerToken request
    (pk, payload) <- checkJWitness
    educatorAuthToken <- maybe mempty pure $ decodeStrict payload
    -- Check expiration time
    currentTime <- liftIO getCurrentTime
    let NumericDate expirationTime = eatExp educatorAuthToken
    guard (currentTime < addUTCTime authTimeout expirationTime)
    -- Remember about timing attacks
    guard (pk `constTimeEq` mpk)
    let ealData = eatData educatorAuthToken
    return $ EducatorAuthLogin {..}

educatorAuthLoginSimple :: EducatorUUID -> EducatorAuthLogin
educatorAuthLoginSimple eadId = EducatorAuthLogin {..}
  where
    ealData = EducatorAuthData {..}
    ealToken = mempty

---------------------------------------------------------------------------
-- No auth
---------------------------------------------------------------------------

type instance NoAuthData "multi-educator" = EducatorAuthLogin


---------------------------------------------------------------------------
-- Documentation
---------------------------------------------------------------------------

instance AuthHasSwagger MultiEducatorAuth where
    authSecurityDoc = jwtSecurityDoc multieducatorAuthDocDesc

multieducatorAuthDocDesc :: Text
multieducatorAuthDocDesc =
    $(embedResourceStringFile $ foldr1 (</>)
        [ "specs"
        , "disciplina"
        , "multi-educator"
        , "api"
        , "authentication.md"
        ]
     )
