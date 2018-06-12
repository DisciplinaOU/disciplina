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
module Disciplina.Launcher.Mode
       (
         -- * Constraints
         BasicWorkMode

         -- * Implementations
       , RealMode
       , NodeContext (..)
       , CustomContext
       , ncCustomCtx

       , -- * Misc
         FormNodeContext (..)
       ) where

import Universum

import Control.Lens (makeLenses)
import Control.Monad.Trans.Control (MonadBaseControl)
import Disciplina.Async (Forall, Pure)
import Ether.Internal (HasLens (..))
import System.Wlog (HasLoggerName (..), LoggerName, WithLogger)
import UnliftIO (MonadUnliftIO)

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

---------------------------------------------------------------------
-- WorkMode implementations
---------------------------------------------------------------------

-- | For node role returns related context.
type family CustomContext r = cc | cc -> r

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

---------------------------------------------------------------------
-- Misc
---------------------------------------------------------------------

-- | Forms context using given pack of resources.
class FormNodeContext res ctx | ctx -> res, res -> ctx where
    formNodeContext :: res -> IO ctx

