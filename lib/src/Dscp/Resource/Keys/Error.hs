{-# LANGUAGE StrictData #-}

module Dscp.Resource.Keys.Error
    ( KeyInitError (..)
    , rewrapKeyIOErrors
    ) where

import qualified Data.Text.Buildable
import Fmt ((+|), (|+))
import qualified Text.Show

import Dscp.Crypto (DecryptionError)
import Dscp.Util (wrapRethrow)

-- | Exception during secret key extraction from storage.
data KeyInitError
    = SecretWrongPassPhraseError DecryptionError
    | SecretParseError Text
    | SecretFileExistsError FilePath
    | SecretIOError Text

instance Show KeyInitError where
    show = toString . pretty

instance Buildable KeyInitError where
    build = \case
        SecretWrongPassPhraseError password ->
            "Wrong password for educator key storage provided ("+|password|+")"
        SecretParseError _ ->
            "Invalid educator secret key storage format"
        SecretFileExistsError path ->
            "Cannot create new secret store, file already exists: "+|path|+""
        SecretIOError msg ->
            "Some I/O error occured: "+|msg|+""

instance Exception KeyInitError

rewrapKeyIOErrors :: MonadCatch m => m a -> m a
rewrapKeyIOErrors = wrapRethrow @SomeException (SecretIOError . show)
