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
    = SecretDecryptionError DecryptionError
    | SecretParseError Text
    | SecretConfMismatch Text
    | SecretIOError Text
    | SecretFileModeError Text

instance Show KeyInitError where
    show = toString . pretty

instance Buildable KeyInitError where
    build = \case
        SecretDecryptionError msg ->
            "Error while decrypting educator key: "+|msg|+
            ". Either retype password or" +|
            "make sure educator.key file corresponds to the right account"
        SecretParseError _ ->
            "Invalid educator secret key storage format"
        SecretConfMismatch msg ->
            "Configuration/CLI params mismatch: "+|msg|+""
        SecretIOError msg ->
            "Some I/O error occured: "+|msg|+""
        SecretFileModeError msg ->
            "File permission error: "+|msg|+""

instance Exception KeyInitError

rewrapKeyIOErrors :: MonadCatch m => m a -> m a
rewrapKeyIOErrors = wrapRethrow @SomeException (SecretIOError . show)
