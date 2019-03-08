{-# LANGUAGE GADTs #-}

module Dscp.MultiEducator.Launcher.Educator.Context
    ( EducatorContextUsers
    , LoadedEducatorContext (..)
    , MaybeLoadedEducatorContext (..)
    , _FullyLoadedEducatorContext
    ) where

import Control.Lens (makePrisms)
import UnliftIO.Async (Async)

import qualified Dscp.Educator.Config as E
import qualified Dscp.Educator.Launcher.Mode as E

type EducatorContextUsers = Set (Async ())

-- | Context and related stuff of a single educator.
data LoadedEducatorContext where
    LoadedEducatorContext
        :: E.HasEducatorConfig
        => { lecCtx :: E.EducatorContext
             -- ^ The context itself.
           }
        -> LoadedEducatorContext

data MaybeLoadedEducatorContext
      -- | Educator context is yet being loaded.
    = YetLoadingEducatorContext
      -- | Educator context has been loaded.
    | FullyLoadedEducatorContext LoadedEducatorContext

makePrisms ''MaybeLoadedEducatorContext
