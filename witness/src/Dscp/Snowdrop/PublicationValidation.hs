{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dscp.Snowdrop.PublicationValidation
       ( validatePublication
       ) where

import qualified Data.List as List
import qualified Data.Map as Map
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

        let (authorAccM :: Maybe (AccountId, Account)) =
              let isAuthor (AccountInIds accId, _) = accId == aAccountId
                  isAuthor _                       = False
              in List.find isAuthor (Map.toList changes) <&> \case
                     (AccountInIds accId, Upd (AccountOutVal it)) -> (accId, it)
                     _ -> error "preValidatePub: author acc is not Upd"
        -- The amount of money spent from author's account
        let feesChosen = case snd <$> authorAccM of
                Nothing     -> 0
                Just newAcc -> aBalance aAccount - aBalance newAcc

        when (feesChosen < fromIntegral (coinToInteger $ unFees aMinFees)) $
            throwLocalError PublicationFeeIsTooLow

        when (aBalance aAccount < feesChosen) $
            throwLocalError PublicationCantAffordFee

        let prevHash = privHeader ^. pbhPrevBlock
        let (prevHashM :: Maybe PrivateHeaderHash) =
                prevHash <$ guard (prevHash /= genesisHeaderHash authorId)

        -- Checking that prevBlockHash matches the one from storage.
        lastPub <- queryOne (PublicationsOf authorId)
        () <- check (fmap unLastPublication lastPub == prevHashM)
                    PublicationPrevBlockIsIncorrect

        let phHash = hash privHeader

        -- Getting actual changes
        let hd  = proj =<< inj (PublicationHead phHash) `Map.lookup` changes
        let box = proj =<< inj (PublicationsOf  authorId)          `Map.lookup` changes

        () <- mconcat
            [ -- Check that we continue the chain correctly.
              check (hd == Just (New (PublicationNext prevHashM)))
                  PublicationIsBroken

            , -- Check that we move head pointer correctly.
              if | isNothing lastPub ->
                    -- The pointer is set in the first time here?
                    check (box == Just (New (LastPublication phHash))) $
                        PublicationIsBroken

                 | otherwise ->
                    -- The pointer is moved to the correct destination here?
                    check (box == Just (Upd (LastPublication phHash))) $
                        PublicationIsBroken
            ]

        return ()
