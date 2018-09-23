module Dscp.Resource.Keys.Types
    ( BaseKeyParams (..)
    , CommitteeParams (..)

    , KeyResources (..)
    , krSecretKey
    , krPublicKey
    , getSecretKey
    , getPublicKey

    , KeyJson (..)
    , KeyfileContent

    , ourSecretKey
    , ourPublicKey
    ) where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Loot.Base.HasLens (HasLens', lensOf)

import Dscp.Core.Aeson ()
import Dscp.Core.Governance (CommitteeSecret (..))
import Dscp.Crypto (Encrypted, PassPhrase, PublicKey, SecretKey)
import Dscp.Util.Aeson (Base64Encoded, CustomEncoding (..), Versioned)

-- | Contains all parameters required for manipulating with secret key.
data BaseKeyParams = BaseKeyParams
    { bkpPath       :: !(Maybe FilePath)
      -- ^ Path to file with secret key.
      -- If not specified, some default OS-dependent path is used.
    , bkpGenNew     :: !Bool
      -- ^ When 'True', file with secret key is expected to be
      -- absent and will be generated from scratch.
      -- When 'False', file should be present and it will be used.
    , bkpPassphrase :: !(Maybe PassPhrase)
      -- ^ Password of encrypted secret key stored on disk.
    } deriving (Show)

-- | In case of committee governance, these keys help us to generate
-- keys.
data CommitteeParams
    = CommitteeParamsOpen { cpParticipantN :: Integer }
      -- ^ In open committee you become participant n/N.
    | CommitteeParamsClosed { cpParticipantN :: Integer
                            , cpSecret       :: CommitteeSecret }
      -- ^ In closed committee you should provide a (common) secret
      -- and your index.
    deriving (Show)

-- | Context providing access to secret key.
--
-- For now we assume secret key to be read-only.
-- Also we assume applicatiion to be autonomous, so secret key is kept
-- unencrypted during the whole operation of application.
--
-- It may be used for various purposes, in order not to mix them we carry
-- phantom type in it.
data KeyResources who = KeyResources
    { _krSecretKey :: !SecretKey
      -- ^ Some secret key.
    , _krPublicKey :: !PublicKey
      -- ^ Corresponding public key, must be exactly equal to 'toPublic sk'.
    }

makeLenses ''KeyResources

getSecretKey
    :: forall node ctx m.
       (MonadReader ctx m, HasLens' ctx (KeyResources node))
    => m SecretKey
getSecretKey = view $ lensOf @(KeyResources node) . krSecretKey

getPublicKey
    :: forall node ctx m.
       (MonadReader ctx m, HasLens' ctx (KeyResources node))
    => m PublicKey
getPublicKey = view $ lensOf @(KeyResources node) . krPublicKey

---------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------

-- | Intermediate form of 'KeyResources' for JSON serialization.
data KeyJson = KeyJson
    { kjEncSecretKey :: CustomEncoding Base64Encoded (Encrypted SecretKey)
    } deriving (Eq, Show)

-- | What exactly lies in the store.
type KeyfileContent = Versioned KeyJson

instance ToJSON KeyJson where
    toJSON KeyJson{..} = object
        [ "secret" .= kjEncSecretKey
        ]

instance FromJSON KeyJson where
    parseJSON = withObject "secret storage" $ \o -> do
        kjEncSecretKey <- o .: "secret"
        return KeyJson{..}

-- | Instances for config params related to keys.
instance FromJSON CommitteeParams where
  parseJSON = withObject "committee seed params" $ \o -> do
    (committeeParamsType :: Text) <- o .: "type"
    case committeeParamsType of
        "committeeOpen" -> do
            cpParticipantN <- o .: "n"
            return $ CommitteeParamsOpen {..}
        "committeeClosed" -> do
            cpParticipantN <- o .: "n"
            cpSecret <- o .: "secret"
            return $ CommitteeParamsClosed {..}
        _ -> fail "Governance type is invalid"

deriveJSON defaultOptions ''BaseKeyParams

---------------------------------------------------------------------
-- HasLens
---------------------------------------------------------------------

-- | Get stored secret key. Parametrized as soon as we may keep several
-- different key resources.
ourSecretKey
    :: forall node ctx m.
       (HasLens' ctx (KeyResources node), MonadReader ctx m)
    => m SecretKey
ourSecretKey = view $ lensOf @(KeyResources node) . krSecretKey

ourPublicKey
    :: forall node ctx m.
       (HasLens' ctx (KeyResources node), MonadReader ctx m)
    => m PublicKey
ourPublicKey = view $ lensOf @(KeyResources node) . krPublicKey
