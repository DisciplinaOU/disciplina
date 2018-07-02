module Dscp.Educator.Secret.Real.Types
    ( EducatorSecretParams (..)
    , EducatorSecret (..)
    , EducatorSecretJson (..)
    , KeyfileContent
    ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Test.QuickCheck (Arbitrary (..))

import Dscp.Crypto (Encrypted (..), PassPhrase, SecretKey)
import Dscp.Educator.Secret.Types (EducatorSecret (..))
import Dscp.Util.Aeson (AsByteString (..), Base64Encoded, Versioned)

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

---------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------

-- | Intermediate form of 'EducatorSecret' for JSON serialization.
data EducatorSecretJson = EducatorSecretJson
    { esjEncSecretKey :: Encrypted SecretKey
    } deriving (Eq, Show)

instance Arbitrary EducatorSecretJson where
    arbitrary = EducatorSecretJson <$> arbitrary

-- | What exactly lies in the store.
type KeyfileContent = Versioned EducatorSecretJson

instance ToJSON EducatorSecretJson where
    toJSON EducatorSecretJson{..} = object
        [ "educator_secret" .= fmap (AsByteString @Base64Encoded) esjEncSecretKey
        ]

instance FromJSON EducatorSecretJson where
    parseJSON = withObject "educator secret storage" $ \o -> do
        rawESK <- o .: "educator_secret"
        let esjEncSecretKey = fmap @Encrypted (getAsByteString @Base64Encoded) rawESK
        return EducatorSecretJson{..}
