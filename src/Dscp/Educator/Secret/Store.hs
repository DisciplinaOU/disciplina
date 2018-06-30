-- | Functions to work with educator secret key storage.

module Dscp.Educator.Secret.Store
    ( readEducatorSecret
    ) where

import Dscp.Educator.Secret.Types (EducatorSecret, EducatorSecretParams)

readEducatorSecret :: EducatorSecretParams -> m EducatorSecret
readEducatorSecret = error "Not implemented"
