{-# LANGUAGE DataKinds #-}

-- | Account-style validation. Copied from
-- 'Snowdrop.Model.State.Accounting.Account'. It'll be brought back to
-- snowdrop at some point :yao:

module Dscp.Snowdrop.Util
       ( requirePart
       , assertExists
       , nothingToLocalError
       , check
       ) where

import Control.Monad.Error.Class (MonadError)
import Snowdrop.Core (ERoComp, HasKeyValue, StatePException, queryOne, validateIff)
import Snowdrop.Util

infix 1 `check`
check :: (Monoid a, HasException e e1) => Bool -> e1 -> ERoComp e id value ctx a
check = flip validateIff

-- | Require that whole projects into part or throw error.
requirePart :: (HasReview e e1, MonadError e m, HasPrism s hash) => s -> e1 -> m hash
requirePart whole message = maybe (throwLocalError message) pure $ proj whole

-- | Require that id exists in a database or throw error.
assertExists ::
       ( HasExceptions e '[ e1, StatePException]
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
