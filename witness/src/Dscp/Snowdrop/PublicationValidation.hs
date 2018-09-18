{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dscp.Snowdrop.PublicationValidation
       ( validatePublication
       ) where

import Data.Map as Map (lookup)
import Snowdrop.Core (PreValidator (..), StateTx (..), StateTxType (..), Validator, ValueOp (..),
                      changeSet, mkValidator, queryOne)
import Snowdrop.Util

import Dscp.Core
import Dscp.Crypto
import qualified Dscp.Crypto as DC (PublicKey)
import Dscp.Snowdrop.AccountValidation
import Dscp.Snowdrop.Configuration
import Dscp.Snowdrop.Storage.Types
import Dscp.Snowdrop.Types


validatePublication ::
       forall ctx.
       ( HasPrism Proofs (PersonalisedProof PublicationTxId PrivateBlockHeader)
       , HasGetter DC.PublicKey Address
       , HasPrism Proofs PublicationTxId
       , CanVerifyPayload PublicationTxId PrivateBlockHeader
       )
    => Validator Exceptions Ids Proofs Values ctx
validatePublication = mkValidator ty [preValidatePublication]
  where
    ty = StateTxType $ getId (Proxy @TxIds) PublicationTxTypeId

preValidatePublication ::
       forall ctx.
       ( HasPrism Proofs (PersonalisedProof PublicationTxId PrivateBlockHeader)
       , HasGetter DC.PublicKey Address
       , HasPrism Proofs PublicationTxId
       , CanVerifyPayload PublicationTxId PrivateBlockHeader
       )
    => PreValidator Exceptions Ids Proofs Values ctx
preValidatePublication =
    PreValidator $ \_trans@ StateTx {..} -> do

        -- Retrieving our Publication payload from proof
        (AccountId authorId, _, privHeader) <- authenticate @PublicationTxId txProof
        let prevHash = privHeader ^. pbhPrevBlock
        let (prevHashM :: Maybe PrivateHeaderHash) =
                prevHash <$ guard (prevHash /= genesisHeaderHash)

        -- Checking that prevBlockHash matches the one from storage.
        lastPub <- queryOne (PublicationsOf authorId)
        () <- check (fmap (hash . unLastPublication) lastPub == prevHashM)
                    PublicationPrevBlockIsIncorrect

        let changes = changeSet txBody

        -- Getting actual changes
        let hd  = proj =<< inj (PublicationHead (hash privHeader)) `Map.lookup` changes
        let box = proj =<< inj (PublicationsOf  authorId)          `Map.lookup` changes

        () <- mconcat
            [ -- Check that we continue the chain correctly.
              check (hd == Just (New (PublicationNext prevHashM)))
                  PublicationIsBroken

            , -- Check that we move head pointer correctly.
              if | isNothing lastPub ->
                    -- The pointer is set in the first time here?
                    check (box == Just (New (LastPublication privHeader))) $
                        PublicationIsBroken

                 | otherwise ->
                    -- The pointer is moved to the correct destination here?
                    check (box == Just (Upd (LastPublication privHeader))) $
                        PublicationIsBroken
            ]

        return ()
