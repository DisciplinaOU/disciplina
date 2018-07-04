
module Dscp.Educator.Secret.Real.Error
    ( EducatorSecretError (..)
    , rewrapSecretIOError
    ) where

import qualified Data.Text.Buildable
import Fmt ((+|), (|+))
import qualified Text.Show

import Dscp.Crypto (DecryptionError)
import Dscp.Util (wrapRethrowIO)

-- | Exception during secret key extraction from storage.
data EducatorSecretError
    = SecretWrongPassPhraseError DecryptionError
    | SecretDeserialisationError Text
    | SecretFileExistsError FilePath
    | SecretIOError Text
    | SecretAppDirUnaccessible Text

instance Show EducatorSecretError where
    show = toString . pretty

instance Buildable EducatorSecretError where
    build = \case
        SecretWrongPassPhraseError password ->
            "Wrong password for educator key storage provided ("+|password|+")"
        SecretDeserialisationError _ ->
            "Invalid educator secret key storage format"
        SecretFileExistsError path ->
            "Cannot create new educator secret store, file already exists: \
            \"+|path|+""
        SecretIOError msg ->
            "Some I/O error occured: "+|msg|+""
        SecretAppDirUnaccessible msg ->
            "No path for secret key was specified; tried home directory, \
            \but encountered error: "+|msg|+""

instance Exception EducatorSecretError

rewrapSecretIOError :: (MonadIO m, MonadCatch m) => IO a -> m a
rewrapSecretIOError = wrapRethrowIO @SomeException (SecretIOError . show)
