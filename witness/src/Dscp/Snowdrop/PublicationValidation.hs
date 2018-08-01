{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dscp.Snowdrop.PublicationValidation
       ( validatePublication
       , Publication (..)
       , PublicationValidationError (..)
       , PublicationTx (..)
       , PublicationTxTypeId (..)
       ) where

import Data.Map as Map (lookup)

import Dscp.Core (Address)
import Dscp.Core.Foundation.Transactions
import Dscp.Core.Foundation.Transactions (Publication (..))
import Dscp.Crypto (PublicKey)
import Dscp.Snowdrop.AccountValidation
import Dscp.Snowdrop.Configuration
import Dscp.Snowdrop.Types (PublicationTxTypeId (..), PublicationValidationError (..))
import Snowdrop.Model.State.Core (PreValidator (..), StateTx (..), StateTxType (..), Validator,
                                  mkValidator, queryOne)

import Snowdrop.Util

validatePublication
    :: forall ctx
    .  HasPrism Proofs (PersonalisedProof PublicationTxId Publication)
    => HasGetter PublicKey Address
    => HasPrism Proofs PublicationTxId
    => CanVerifyPayload PublicationTxId Publication
    => Validator Exceptions Ids Proofs Values ctx
validatePublication = mkValidator ty [preValidatePublication]
  where
    ty = StateTxType $ getId (Proxy @TxIds) PublicationTxTypeId

preValidatePublication
    :: forall ctx
    .  HasPrism Proofs (PersonalisedProof PublicationTxId Publication)
    => HasGetter PublicKey Address
    => HasPrism Proofs PublicationTxId
    => CanVerifyPayload PublicationTxId Publication
    => PreValidator Exceptions Ids Proofs Values ctx
preValidatePublication =
    PreValidator $ \_trans@ StateTx {..} -> do

        -- Retrieving our Publication payload from proof
        (AccountId authorId, _, Publication {..}) <- authenticate @PublicationTxId txProof

        -- Checking that prevBlockHash matches the one from storage.
        -- This can be written as:
        --
        -- res <- queryOne ...
        -- validateUnless (getLastPub <$> res == pPrevBlockHash) Pub...
        --
        -- ... but this shows the intent more clear.
        lastPub <- queryOne (PublicationsOf authorId) >>= \it -> do
            () <- case it of
                Just (LastPublication theLast) -> do
                    check (Just theLast == pPreviousBlockHash) $
                        PublicationPrevBlockIsIncorrect


                Nothing -> do
                    check (Nothing == pPreviousBlockHash) $
                        PublicationPrevBlockIsIncorrect

            return it

        let changes = changeSet txBody

        -- | Getting actual changes
        let hd  = proj =<< inj (PublicationHead pPrivateBlockHash) `Map.lookup` changes
        let box = proj =<< inj (PublicationsOf  authorId)          `Map.lookup` changes

        () <- mconcat
            [ -- Check that we continue the chain correctly.
              check (hd == Just (New (PublicationNext pPreviousBlockHash))) $
                PublicationIsBroken

            , -- Check that we move head pointer correctly.
              if | isNothing lastPub ->
                    -- The pointer is set in the first time here?
                    check (box == Just (New (LastPublication pPrivateBlockHash))) $
                        PublicationIsBroken

                 | otherwise ->
                    -- The pointer is moved to the correct destination here?
                    check (box == Just (Upd (LastPublication pPrivateBlockHash))) $
                        PublicationIsBroken
            ]

        return ()
