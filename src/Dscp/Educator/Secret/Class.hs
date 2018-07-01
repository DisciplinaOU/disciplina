-- | Constraint providing access to educator's secret key.

module Dscp.Educator.Secret.Class
    ( MonadEducatorSecret (..)
    ) where

import Control.Lens (views)
import Loot.Base.HasLens (HasLens', lensOf)

import Dscp.Crypto.Impl (SecretKey)
import Dscp.Educator.Secret.Types (EducatorSecret (..))
import Dscp.Launcher.Rio (RIO)

class MonadEducatorSecret m where
    -- | Fetch secret key of educator.
    getEducatorSecret :: m SecretKey

instance HasLens' ctx EducatorSecret =>
         MonadEducatorSecret (RIO ctx) where
    getEducatorSecret = views (lensOf @EducatorSecret) esSecretKey
