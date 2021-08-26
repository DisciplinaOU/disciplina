{-# LANGUAGE OverloadedLabels #-}

module Dscp.Resource.Keys.Types
    ( BaseKeyParams
    , BaseKeyParamsRec
    , BaseKeyParamsRecP
    , defaultBaseKeyParams

    , CommitteeParams
    , CommitteeParamsRec
    , CommitteeParamsRecP

    , KeyResources (..)

    , KeyJson (..)
    , KeyfileContent
    , krSecretKeyData
    , krSecretKey
    , krPublicKey

    , ourSecretKey
    , ourPublicKey
    , ourAddress
    , ourSecretKeyData
    ) where

import Control.Lens (Getter, makeLenses, to)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Loot.Base.HasLens (HasLens', lensOf)
import Loot.Config ((::+), (::-), (:::), Config, PartialConfig, option, (?~))

import Dscp.Core.Aeson ()
import Dscp.Core.Foundation.Address
import Dscp.Core.Foundation.Witness
import Dscp.Core.Governance (CommitteeSecret (..))
import Dscp.Crypto (Encrypted, PassPhrase, PublicKey, SecretKey)
import Dscp.Util.Aeson (Base64Encoded, EncodeSerialised (..), Versioned)

-- | Contains all parameters required for manipulating with secret key.
type BaseKeyParams =
   '[ "path"       ::: Maybe FilePath
      -- Path to file with secret key.
      -- If not specified, some default OS-dependent path is used.
    , "genNew"     ::: Bool
      -- When 'True', file with secret key is expected to be
      -- absent and will be generated from scratch.
    , "passphrase" ::: Maybe PassPhrase
      -- When 'False', file should be present and it will be used.
      -- Password of encrypted secret key stored on disk.
    ]

type BaseKeyParamsRec = Config BaseKeyParams
type BaseKeyParamsRecP = PartialConfig BaseKeyParams

defaultBaseKeyParams :: BaseKeyParamsRecP
defaultBaseKeyParams = mempty
    & option #path       ?~ Nothing
    & option #genNew     ?~ False
    & option #passphrase ?~ Nothing

-- | In case of committee governance, these keys help us to generate keys.
-- Note, there is a reason this contains the whole tree and not just its content
-- see 'ConfigMaybe' for an explanation.
type CommitteeParams =
   '[ "params" ::+
       '[ "participantN" ::: Integer
          -- This is necessary to both types of committee
        , "open" ::- '[]
          -- In open committee you become participant n/N.
        , "closed" ::-
           '[ "secret" ::: CommitteeSecret
            ]
          -- In closed committee you should provide a (common) secret and your index.
        ]
    ]

type CommitteeParamsRec = Config CommitteeParams
type CommitteeParamsRecP = PartialConfig CommitteeParams

-- | Context providing access to secret key.
--
-- For now we assume secret key to be read-only.
-- Also we assume applicatiion to be autonomous, so secret key is kept
-- unencrypted during the whole operation of application.
--
-- It may be used for various purposes, in order not to mix them we carry
-- phantom type in it.
newtype KeyResources who = KeyResources
    { _krSecretKeyData :: SecretKeyData
      -- ^ Some secret key.
    }

makeLenses ''KeyResources

krSecretKey :: Getter (KeyResources who) SecretKey
krSecretKey = krSecretKeyData . to skSecret

krPublicKey :: Getter (KeyResources who) PublicKey
krPublicKey = krSecretKeyData . to skPublic

---------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------

-- | Intermediate form of 'KeyResources' for JSON serialization.
data KeyJson = KeyJson
    { kjEncSecretKey :: EncodeSerialised Base64Encoded (Encrypted SecretKey)
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
ourSecretKey = view $ lensOf @(KeyResources node) . krSecretKeyData . to skSecret

ourPublicKey
    :: forall node ctx m.
       (HasLens' ctx (KeyResources node), MonadReader ctx m)
    => m PublicKey
ourPublicKey = view $ lensOf @(KeyResources node) . krSecretKeyData . to skPublic

ourAddress
    :: forall node ctx m.
       (HasLens' ctx (KeyResources node), MonadReader ctx m)
    => m Address
ourAddress = view $ lensOf @(KeyResources node) . krSecretKeyData . to skAddress

ourSecretKeyData
    :: forall node ctx m.
       (MonadReader ctx m, HasLens' ctx (KeyResources node))
    => m SecretKeyData
ourSecretKeyData = view $ lensOf @(KeyResources node) . krSecretKeyData
