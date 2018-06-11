{-# LANGUAGE TemplateHaskell #-}

{- | Module contains the definition of WorkMode and its implementations.

Some notes on architcturial descisions:

* Among the project we use monad stack consisting of @ReaderT ctx m@,
and thus tend to use module for
threading because it perfectly fits for IO-based monad stacks which have no
state embeded into them, i.e. @StM m a ~ a@.
Alternatives are
  * 'Control.Concurrent.Async' for 'IO'
  * 'Control.Concurrent.Async.Lifted.Safe' for monad stack with no embeded state
(@StM m a ~ a@), but we actually we don't need powerful 'MonadBaseControl'
  * 'Control.Concurrent.Async.Lifted' - for monad stack with embeded state,
same thing with it

-}
module Disciplina.Launcher.Mode
       (
         -- * Constraints
         BasicWorkMode
       , EducatorWorkMode
       , WitnessWorkMode

         -- * Implementations
       , RealMode
       , NodeContext (..)
       , CustomContext
       , ncCustomCtx

       , BasicRealMode
       , Basic
       , NoCustomContext (..)

       , EducatorRealMode
       , Educator
       , EducatorCustomContext (..)

       , WitnessRealMode
       , Witness
       , WitnessCustomContext (..)
       ) where

import Universum

import Control.Lens (makeLenses)
import Ether.Internal (HasLens (..))
import System.Wlog (HasLoggerName (..), LoggerName, WithLogger)
import UnliftIO (MonadUnliftIO)

import Disciplina.DB.Class (MonadDB)
import Disciplina.DB.Real.Types (NodeDB)

---------------------------------------------------------------------
-- WorkMode classes
---------------------------------------------------------------------

-- | Set of typeclasses which define basic capabilities of Disciplina node
type BasicWorkMode m =
    ( WithLogger m
    , MonadIO m
    , MonadDB m
    , MonadUnliftIO m  -- allows to use lifted-async
    )

type EducatorWorkMode m =
    ( BasicWorkMode m
    )

type WitnessWorkMode m =
    ( BasicWorkMode m
    )

---------------------------------------------------------------------
-- WorkMode implementations
---------------------------------------------------------------------

-- | For node role returns related context.
type family CustomContext r :: *

-- | Contains basic context suitable for any node + custom context,
-- which depends on node role.
data NodeContext r = NodeContext
    { _ncCustomCtx  :: CustomContext r
    , _ncLoggerName :: LoggerName
    }

makeLenses ''NodeContext

type RealMode r = ReaderT (NodeContext r) IO

instance {-# OVERLAPPING #-} HasLoggerName (RealMode r) where
    askLoggerName = view ncLoggerName
    modifyLoggerName name = local $ ncLoggerName %~ name

-- | Used in cases where any role fits.
data Basic

type instance CustomContext Basic = NoCustomContext
data NoCustomContext = NoCustomContext

type BasicRealMode = RealMode Basic
-- TODO [DSCP-105]: perhaps we don't need these alises, and 'RealMode Someone' is quite telling?

-- | Educator node role.
data Educator

type instance CustomContext Educator = EducatorCustomContext
data EducatorCustomContext = EducatorCustomContext

makeLenses ''EducatorCustomContext

type EducatorRealMode = RealMode Educator

-- | Witness node role.
data Witness

type instance CustomContext Witness = WitnessCustomContext
data WitnessCustomContext = WitnessCustomContext
    { _wcDB :: NodeDB
    }

makeLenses ''WitnessCustomContext

type WitnessRealMode = RealMode Witness

instance HasLens NodeDB (NodeContext Witness) NodeDB where
    lensOf = ncCustomCtx . wcDB
