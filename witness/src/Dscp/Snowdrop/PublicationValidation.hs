{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dscp.Snowdrop.PublicationValidation
       ( validatePublication
       ) where

import qualified Data.Map as Map
import Fmt ((+||), (||+))
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

        let (changes :: Map Ids (ValueOp Values)) = changeSet txBody

        -- Retrieving our Publication payload from proof
        Authenticated {..} <- authenticate @PublicationTxId txProof
        let privHeader = aPayload

        let authorId = unAccountId aAccountId

        let prevHash = privHeader ^. pbhPrevBlock
        let (prevHashM :: Maybe PrivateHeaderHash) =
                prevHash <$ guard (prevHash /= genesisHeaderHash)

        -- Checking that prevBlockHash matches the one from storage.
        lastPub <- queryOne (PublicationsOf authorId)
        () <- check (fmap unLastPublication lastPub == prevHashM)
                    PublicationPrevBlockIsIncorrect

        let phHash = hash privHeader

        -- Getting actual changes
        let hd  = proj =<< inj (PublicationHead authorId phHash) `Map.lookup` changes
        let box = proj =<< inj (PublicationsOf  authorId) `Map.lookup` changes

        () <- mconcat
            [ -- Check that we continue the chain correctly.
              check (hd == Just (New (PublicationNext prevHashM))) $
                  PublicationIsBroken $ "Odd next publication in changeset: " +|| hd ||+ ""

            , -- Check that we move head pointer correctly.
              if | isNothing lastPub ->
                    -- The pointer is set in the first time here?
                    check (box == Just (New (LastPublication phHash))) $
                        PublicationIsBroken $ "Odd chain head in changeset: " +|| box ||+
                                              " (expected New)"

                 | otherwise ->
                    -- The pointer is moved to the correct destination here?
                    check (box == Just (Upd (LastPublication phHash))) $
                        PublicationIsBroken $ "Odd chain head in changeset: " +|| box ||+
                                              " (expected Upd)"
            ]

        return ()
