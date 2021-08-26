{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dscp.Snowdrop.BlockMetaValidation
       ( validateBlockMeta
       ) where

import Snowdrop.Core (PreValidator (..), StateTx (..), StateTxType (..), Validator, mkValidator)
import Snowdrop.Util

import Dscp.Snowdrop.Configuration
import Dscp.Snowdrop.Types


validateBlockMeta
    :: forall ctx.
       Validator Exceptions Ids Proofs Values ctx
validateBlockMeta = mkValidator ty [preValidateBlockMeta]
  where
    ty = StateTxType $ getId (Proxy @TxIds) BlockMetaTxTypeId

preValidateBlockMeta
    :: forall ctx.
       PreValidator Exceptions Ids Proofs Values ctx
preValidateBlockMeta =
    PreValidator $ \_trans@StateTx {..} ->
        -- Cry, snowdrop, cry.

        pass  -- All the validation has happened in expander.
