{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilyDependencies #-}

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
module Dscp.Launcher.Mode
       (
         -- * Constraints
         BasicWorkMode

         -- * RIO monad
       , RIO (..)
       , runRIO
       ) where

import Universum

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Ether.Internal (HasLens)
import Loot.Log (ModifyLogName (..), MonadLogging (..), WithLogging)
import Loot.Log.Rio (LoggingIO)
import qualified Loot.Log.Rio as Rio
import UnliftIO (MonadUnliftIO)

---------------------------------------------------------------------
-- WorkMode classes
---------------------------------------------------------------------

-- | Set of typeclasses which define basic capabilities of Disciplina node
type BasicWorkMode m =
    ( WithLogging m
    , MonadIO m
    , MonadUnliftIO m  -- allows to use lifted-async
    )

---------------------------------------------------------------------
-- WorkMode implementations
---------------------------------------------------------------------

{- | Conventional "ReaderT over IO" monad stack.

Lootbox bases on 'caps' library which allows the only 'ReaderT' instance for
used typeclasses, e.g.
@instance (r ~ Capabilities caps) => MonadLogging (ReaderT r IO)@.
To avoid instances overlapping, we use this wrapper.

This also allows us to remorselessly define one global
@instance HasLens Smth ctx Smth => MonadSmth (RIO ctx)@ per every @Smth@.
-}
newtype RIO ctx a = RIO { unRIO :: ReaderT ctx IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader ctx,
              MonadThrow, MonadCatch, MonadMask)

runRIO :: MonadIO m => ctx -> RIO ctx a -> m a
runRIO ctx (RIO act) = liftIO $ runReaderT act ctx

instance HasLens LoggingIO ctx LoggingIO =>
         MonadLogging (RIO ctx) where
    log = Rio.defaultLog
    logName = Rio.defaultLogName

instance HasLens LoggingIO ctx LoggingIO =>
         ModifyLogName (RIO ctx) where
    modifyLogNameSel = Rio.defaultModifyLogNameSel
