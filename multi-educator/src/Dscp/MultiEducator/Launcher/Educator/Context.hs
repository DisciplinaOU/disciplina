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

type EducatorContextUsers = Set (Async ())

newtype EducatorContextKeeper = EducatorContextKeeper (Async Void)

-- | Context and related stuff of a single educator.
data LoadedEducatorContext where
    LoadedEducatorContext
        :: E.HasEducatorConfig
        => { lecCtx :: E.EducatorContext
             -- ^ The context itself.
           , lecContextKeeper :: EducatorContextKeeper
             -- ^ Handler to context and resources keeping thread.
           , lecLastActivity :: Timestamp
             -- ^ Last user request time.
           , lecUsers :: EducatorContextUsers
             -- ^ Threads which take use of the context currently.
           , lecNoFurtherUsers :: Bool
             -- ^ Whether new context users are allowed.
             -- You may set this to @True@ upon context termination if
             -- only STM context is yet available.
           }
        -> LoadedEducatorContext

lecUsersL :: Lens' LoadedEducatorContext EducatorContextUsers
lecUsersL = lens lecUsers (\ctx usrs -> ctx{ lecUsers = usrs })

data MaybeLoadedEducatorContext
      -- | Educator context is getting prepared.
    = YetLoadingEducatorContext
      -- | Educator context has been loaded.
    | FullyLoadedEducatorContext LoadedEducatorContext
      -- | Educator context is currently being unloaded.
    | TerminatingEducatorContext EducatorContextKeeper

makePrisms ''MaybeLoadedEducatorContext
