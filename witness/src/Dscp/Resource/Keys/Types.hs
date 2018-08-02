module Dscp.Resource.Keys.Types
    ( BaseKeyParams (..)
    , CommitteeParams (..)
    , WitnessKeyParams (..)
    , EducatorKeyParams (..)

    , KeyResources (..)
    , krSecretKey
    , krPublicKey

    , KeyJson (..)
    , KeyfileContent

    , ourSecretKey
    , ourPublicKey
    ) where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Loot.Base.HasLens (HasLens', lensOf)

import Dscp.Core.Governance (CommitteeSecret (..))
import Dscp.Crypto (Encrypted, PassPhrase, PublicKey, SecretKey)
import Dscp.Util.Aeson (Versioned)

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

-- | Witness key parameters.
data WitnessKeyParams = WitnessKeyParams
    { wkpBase      :: !BaseKeyParams
    , wkpCommittee :: !(Maybe CommitteeParams)
      -- ^ Optional committee params which may alter key generation.
    } deriving (Show)

-- | Educator key parameters (move to educator/).
newtype EducatorKeyParams = EducatorKeyParams
    { unEducatorKeyParams :: BaseKeyParams
    } deriving (Show)

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

---------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------

-- | Intermediate form of 'KeyResources' for JSON serialization.
data KeyJson = KeyJson
    { kjEncSecretKey :: Encrypted SecretKey
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
