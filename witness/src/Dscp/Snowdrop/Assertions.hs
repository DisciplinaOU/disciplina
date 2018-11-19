
module Dscp.Snowdrop.Assertions where

import Control.Monad.Error.Class (MonadError)

import Snowdrop.Core(
    queryOne, ERoComp, StatePException, HasKeyValue,
    validateIff)
import Snowdrop.Util

assertSigned
    :: ( VerifySign sigScheme a
       , HasReview e e1
       , MonadError e m
       )
    => WithSignature sigScheme a
    -> e1
    -> m a
assertSigned WithSignature {..} message = do
    () <- validateIff message $
        verifySignature wsPublicKey wsBody wsSignature

    return wsBody

infix 1 `check`
check :: (Monoid a, HasException e e1) => Bool -> e1 -> ERoComp e id value ctx a
check = flip validateIff

-- | Require that whole projects into part or throw error.
requirePart :: (HasReview e e1, MonadError e m, HasPrism s hash) => s -> e1 -> m hash
requirePart whole message = maybe (throwLocalError message) pure $ proj whole

-- | Require that id exists in a database or throw error.
assertExists ::
       forall id id' value a ctx e e1
    .  ( HasExceptions e '[ e1, StatePException]
       , Ord id
       , Ord id'
       , HasKeyValue id value id' a
       )
    => id'
    -> e1
    -> ERoComp e id value ctx a
assertExists thing message =
    maybe (throwLocalError message) pure =<< queryOne thing

nothingToLocalError
    :: HasReview e e1
    => e1 -> Maybe a -> ERoComp e id value ctx a
nothingToLocalError err mThing = maybe (throwLocalError err) pure mThing

-- | Require that id does not exists in a database or throw error.
assertAbsence ::
       forall id id' value value' a ctx e e1
    .  ( HasExceptions e '[ e1, StatePException]
       , Ord id
       , Ord id'
       , HasKeyValue id value id' value'
       , Monoid a
       )
    => id'
    -> e1
    -> ERoComp e id value ctx a
assertAbsence thing message =
    queryOne thing
        >>= maybe
            (pure mempty)
            (const $ throwLocalError message)
