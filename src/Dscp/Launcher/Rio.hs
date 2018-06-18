-- | RIO monad we may wish to use all over the project.

module Dscp.Launcher.Rio
    ( RIO (..)
    , runRIO
    ) where

import Universum

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Ether.Internal (HasLens)
import Loot.Log (ModifyLogName (..), MonadLogging (..))
import Loot.Log.Rio (LoggingIO)
import qualified Loot.Log.Rio as Rio

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
