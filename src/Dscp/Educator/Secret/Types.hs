module Dscp.Educator.Secret.Types
    ( EducatorSecretParams (..)
    , EducatorSecret (..)
    ) where

import Dscp.Crypto (PassPhrase, SecretKey)

-- | Contains all parameters required for manipulating with secret key.
data EducatorSecretParams = EducatorSecretParams
    { espPath     :: !(Maybe FilePath)
      -- ^ Path to file with secret key.
      -- If not specified, some default OS-dependent path is used.
    , espGenNew   :: !Bool
      -- ^ When 'True', file with secret key is expected to be
      -- absent and will be generated from scratch.
      -- When 'False', file should be present and it will be used.
    , espPassword :: !PassPhrase
      -- ^ Password from encrypted secret key stored on disk.
    }

-- | Context providing access to educator secret key.
--
-- For now we assume secret key to be read-only.
-- Also we assume applicatiion to be autonomous, so secret key is kept
-- unencrypted during the whole operation of application.
data EducatorSecret = EducatorSecret
    { escSecretKey :: !SecretKey
    }
