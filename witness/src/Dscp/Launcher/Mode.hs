{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{- | Module contains the definition of WorkMode and its implementations.

Some notes on architcturial descisions:

* Among the project we use monad stack consisting of @ReaderT ctx m@,
and thus tend to use 'UnliftIO.Async' module for threading because it
perfectly fits for IO-based monad stacks which have no state embeded
into them, i.e. @StM m a ~ a@.

Alternatives are
  * 'Control.Concurrent.Async' for 'IO'
  * 'Control.Concurrent.Async.Lifted.Safe' for monad stack with no embeded state
(@StM m a ~ a@), but we actually we don't need powerful 'MonadBaseControl'
  * 'Control.Concurrent.Async.Lifted' - for monad stack with embeded state,
same thing with it

-}
module Dscp.Launcher.Mode
       (
         -- * Constraints
         BasicWorkMode

       , ModeEnv (..)
       , RealModeProof
       , TestModeProof
       ) where

import Data.Type.Equality ((:~:))
import Loot.Log (WithLogging)
import UnliftIO (MonadUnliftIO)

import Dscp.Util

---------------------------------------------------------------------
-- WorkMode classes
---------------------------------------------------------------------

-- | Set of typeclasses which define basic capabilities of Disciplina node
type BasicWorkMode m =
    ( WithLogging m
    , MonadIO m
    , MonadUnliftIO m  -- allows to use lifted-async
    , MonadMask m
    )

-- | Parameter for distinguishing producation and test constraints.
data ModeEnv = RealMode | TestMode

type RealModeProof e = (e :~: 'RealMode, Refuted (e :~: 'TestMode))
type TestModeProof e = (e :~: 'TestMode, Refuted (e :~: 'RealMode))
