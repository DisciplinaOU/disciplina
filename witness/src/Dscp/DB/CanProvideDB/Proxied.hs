
module Dscp.DB.CanProvideDB.Proxied () where

import Loot.Base.HasLens (HasLens', lensOf)

import Dscp.DB.CanProvideDB.Class

instance (MonadReader ctx m, HasLens' ctx Plugin) => ProvidesPlugin m where
    providePlugin = view (lensOf @Plugin)
