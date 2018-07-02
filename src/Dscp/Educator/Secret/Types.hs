
module Dscp.Educator.Secret.Types
    ( EducatorSecret (..)
    ) where

import Dscp.Crypto (SecretKey)

-- | Context providing access to educator secret key.
--
-- For now we assume secret key to be read-only.
-- Also we assume applicatiion to be autonomous, so secret key is kept
-- unencrypted during the whole operation of application.
data EducatorSecret = EducatorSecret
    { esSecretKey :: !SecretKey
    }
