module Dscp.Util.Rethrow
    ( MonadRethrow (..)
    , rethrow
    , rethrowJust
    , UseMonadRethrow

      -- * Testing
    , ExpectedException (..)
    , expectRethrowing
    , expectAnyRethrowing
    , expectJustRethrowing
    , expectPrismRethrowing
    , throwsOnlyExpected
    ) where

import Control.Lens (LensLike')
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Test.QuickCheck (Property, Testable, property)

-- | Allows to map thrown exceptions.
-- Suitable when defining `MonadCatch` is undesired.
class MonadThrow m => MonadRethrow m where
    -- | A version of 'catch' which cannot completely handle exceptions,
    -- it has to throw them further.
    -- Asynchronous exceptions are passed.
    catchHot :: Exception e => m a -> (e -> m Void) -> m a

    default catchHot :: (MonadCatch m, Exception e) => m a -> (e -> m Void) -> m a
    catchHot action handler = catch action (vacuous . handler)
    {-# NOINLINE catchHot #-}

instance MonadRethrow IO
instance MonadCatch m => MonadRethrow (ReaderT r m)

-- | Catches synchronous errors, maps them and throws further.
rethrow
    :: (MonadRethrow m, Exception e1, Exception e2)
    => (e1 -> e2) -> m a -> m a
rethrow wrap action = catchHot action (throwM . wrap)

-- | Maps exceptions; those for which mapping fails are passed.
rethrowJust
    :: (MonadRethrow m, Exception e1, Exception e2)
    => (e1 -> Maybe e2) -> m a -> m a
rethrowJust wrap action =
    catchHot action (\e -> maybe (throwM e) throwM (wrap e))

-- | Useful for `MonadCatch` implementation.
type family UseMonadRethrow m :: Constraint where
    UseMonadRethrow m =
        ( MonadThrow m
        , TypeError
            ('Text "Using MonadCatch instance is not allowed here" ':$$:
             'Text "Consider using `rethrow` function instead")
        )

----------------------------------------------------------------------------
-- Testing
----------------------------------------------------------------------------

-- | Sometimes in test case you want to check that a proper exception is
-- thrown but you have only @MonadRethrow m@ context.
-- In this case you can rethrow 'ExpectedException' and match against it later.
data ExpectedException = ExpectedException
    deriving (Show)

instance Exception ExpectedException

-- | When you expect an exception but it does not happen, you can throw this
-- thing.
data UnexpectedlyNoException = UnexpectedlyNoException
    deriving (Show)

instance Exception UnexpectedlyNoException

-- | Expect that only the given exception is thrown.
expectRethrowing
    :: forall e m a.
       (MonadRethrow m, Exception e)
    => m a -> m Void
expectRethrowing action =
    rethrow (\(_ :: e) -> ExpectedException) action
    *> throwM UnexpectedlyNoException

-- | Expect that any synchronous exception is thrown.
expectAnyRethrowing
    :: MonadRethrow m
    => m a -> m Void
expectAnyRethrowing = expectRethrowing @SomeException

-- | Expect that an exception returning 'Just' on the given predicate is thrown.
expectJustRethrowing
    :: (MonadRethrow m, Exception e)
    => (e -> Maybe e') -> m a -> m Void
expectJustRethrowing match action =
    rethrowJust (\e -> match e $> ExpectedException) action
    *> throwM UnexpectedlyNoException

-- | Expect that exception matching the given prism is thrown.
expectPrismRethrowing
    :: (MonadRethrow m, Exception e)
    => LensLike' (Const $ First e') e e' -> m a -> m Void
expectPrismRethrowing pri = expectJustRethrowing (^? pri)

-- | Fail only when an exception is thrown, and that's not 'ExpectedException'.
throwsOnlyExpected :: (MonadCatch m, Testable prop) => m prop -> m Property
throwsOnlyExpected action =
    catch (property <$> action) $ \ExpectedException -> return $ property ()
