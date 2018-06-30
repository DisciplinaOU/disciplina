
module Dscp.Educator.Secret.Error
    ( EducatorSecretError (..)
    ) where

import qualified Data.Text.Buildable
import Fmt ((+|), (|+))
import qualified Text.Show

-- | Exception during secret key extraction from storage.
data EducatorSecretError
    = SecretWrongPasswordError Double
    | SecretDeserialisationError

instance Show EducatorSecretError where
    show = toString . pretty

instance Buildable EducatorSecretError where
    build = \case
        SecretWrongPasswordError password ->
            "Wrong password for educator key storage provided: "+|password|+""
        SecretDeserialisationError ->
            "Invalid educator secret key storage format"

