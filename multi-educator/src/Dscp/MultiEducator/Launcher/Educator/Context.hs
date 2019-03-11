{-# LANGUAGE GADTs #-}

module Dscp.MultiEducator.Launcher.Educator.Context
    ( EducatorContextUsers
    , EducatorContextKeeper (..)
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

-- | Set of handlers to threads which currently use the context.
type EducatorContextUsers = Set (Async ())

-- | Handler to resources keeping thread.
newtype EducatorContextKeeper = EducatorContextKeeper (Async Void)

-- | Context and related stuff of a single educator.
data LoadedEducatorContext where
    LoadedEducatorContext
        :: E.HasEducatorConfig
        => { lecCtx :: E.EducatorContext
             -- ^ The context itself.
           , lecContextKeeper :: EducatorContextKeeper
             -- ^ Handler to resources keeping thread.
           , lecLastActivity :: Timestamp
             -- ^ Last user request time.
           , lecUsers :: EducatorContextUsers
             -- ^ Threads which take use of the context currently.
           , lecNoFurtherUsers :: Bool
             -- ^ Whether new context users are prohibited.
             -- You may set this to @True@ upon context termination
             -- within STM to provide some atomicity guarantees.
           }
        -> LoadedEducatorContext

lecUsersL :: Lens' LoadedEducatorContext EducatorContextUsers
lecUsersL = lens lecUsers (\ctx usrs -> ctx{ lecUsers = usrs })

data MaybeLoadedEducatorContext
      -- | Educator context is yet being loaded.
    = YetLoadingEducatorContext
      -- | Educator context has been loaded.
    | FullyLoadedEducatorContext LoadedEducatorContext
      -- | Educator context is currently being unloaded.
    | TerminatingEducatorContext EducatorContextKeeper

makePrisms ''MaybeLoadedEducatorContext
