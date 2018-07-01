module Dscp.Educator.Secret.Types
    ( EducatorSecretParams (..)
    , EducatorSecret (..)
    , EducatorSecretJson (..)
    ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))

import Dscp.Crypto (Encrypted (..), PassPhrase, SecretKey)
import Dscp.Util.Aeson (AsByteString (..), Base64)

-- | Contains all parameters required for manipulating with secret key.
data EducatorSecretParams = EducatorSecretParams
    { espPath       :: !(Maybe FilePath)
      -- ^ Path to file with secret key.
      -- If not specified, some default OS-dependent path is used.
    , espGenNew     :: !Bool
      -- ^ When 'True', file with secret key is expected to be
      -- absent and will be generated from scratch.
      -- When 'False', file should be present and it will be used.
    , espPassphrase :: !PassPhrase
      -- ^ Password from encrypted secret key stored on disk.
    }

-- | Context providing access to educator secret key.
--
-- For now we assume secret key to be read-only.
-- Also we assume applicatiion to be autonomous, so secret key is kept
-- unencrypted during the whole operation of application.
data EducatorSecret = EducatorSecret
    { esSecretKey :: !SecretKey
    }

---------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------

-- | Intermediate form of 'EducatorSecret' for JSON serialization.
data EducatorSecretJson = EducatorSecretJson
    { esjEncSecretKey :: Encrypted SecretKey
    }

instance ToJSON EducatorSecretJson where
    toJSON EducatorSecretJson{..} = object
        [ "educator_secret" .= fmap (AsByteString @Base64) esjEncSecretKey
        ]

instance FromJSON EducatorSecretJson where
    parseJSON = withObject "educator secret storage" $ \o -> do
        rawESK <- o .: "educator_secret"
        let esjEncSecretKey = fmap @Encrypted (getAsByteString @Base64) rawESK
        return EducatorSecretJson{..}
