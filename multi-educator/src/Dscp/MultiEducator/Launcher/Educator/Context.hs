{-# LANGUAGE GADTs #-}

module Dscp.MultiEducator.Launcher.Educator.Context
    ( EducatorContextUsers
    , LoadedEducatorContext (..)
    , MaybeLoadedEducatorContext (..)
    , _FullyLoadedEducatorContext
    , lecUsersL
    ) where

import Control.Lens (makePrisms, lens)
import Time (Timestamp)
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
           , lecContextKeeper :: Async Void
             -- ^ Handler to context and resources keeping thread.
           , lecLastActivity :: Timestamp
             -- ^ Last user request time.
           , lecUsers :: EducatorContextUsers
             -- ^ Threads which take use of the context currently.
           }
        -> LoadedEducatorContext

lecUsersL :: Lens' LoadedEducatorContext EducatorContextUsers
lecUsersL = lens lecUsers (\ctx usrs -> ctx{ lecUsers = usrs })

data MaybeLoadedEducatorContext
      -- | Educator context is in transient state (loading or unloading).
    = LockedEducatorContext
      -- | Educator context has been loaded.
    | FullyLoadedEducatorContext LoadedEducatorContext

makePrisms ''MaybeLoadedEducatorContext
