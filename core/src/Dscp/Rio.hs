-- | RIO monad we may wish to use all over the project.

module Dscp.Rio
    ( RIO (..)
    , runRIO
    ) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Fix (MonadFix)
import Loot.Base.HasLens (HasLens (..))
import Loot.Log (ModifyLogName (..), MonadLogging (..))
import Loot.Log.Rio (LoggingIO)
import qualified Loot.Log.Rio as Rio
import UnliftIO (MonadUnliftIO)

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
              MonadThrow, MonadCatch, MonadMask, MonadUnliftIO, MonadFix)

runRIO :: MonadIO m => ctx -> RIO ctx a -> m a
runRIO ctx (RIO act) = liftIO $ runReaderT act ctx

instance HasLens a (a, b) a where
    lensOf = _1
instance HasLens b (a, b) b where
    lensOf = _2

instance HasLens LoggingIO ctx LoggingIO =>
         MonadLogging (RIO ctx) where
    log = Rio.defaultLog
    logName = Rio.defaultLogName

instance HasLens LoggingIO ctx LoggingIO =>
         ModifyLogName (RIO ctx) where
    modifyLogNameSel = Rio.defaultModifyLogNameSel
