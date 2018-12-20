-- | Block-level info validation and expansion.
module Dscp.Snowdrop.Expanders.BlockMeta
    ( expandBlockMetaTx
    , seqExpandersBlockMetaTx
    ) where

import Data.Default (Default (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Fmt ((+|), (|+))
import Serokell.Util (enumerate)

import qualified Snowdrop.Block as SD
import Snowdrop.Core (ChgAccum, ChgAccumCtx, ERoComp, Expander (..), SeqExpanders (..),
                      StateTxType (..), ValueOp (..), mkDiffCS, queryOne)
import Snowdrop.Execution (RestrictCtx, expandUnionRawTxs)
import Snowdrop.Util

import Dscp.Core
import Dscp.Crypto as Dscp
import Dscp.Snowdrop.AccountValidation
import Dscp.Snowdrop.Configuration
import Dscp.Snowdrop.Storage.Types
import Dscp.Snowdrop.Types

expandBlockMetaTx
    :: ( Default (ChgAccum ctx)
       , HasLens ctx RestrictCtx
       , HasLens ctx (ChgAccumCtx ctx)
       , HasCoreConfig
       )
    => BlockMetaTx -> ERoComp Exceptions Ids Values ctx SStateTx
expandBlockMetaTx meta =
    let stateTx = StateTxType $ getId (Proxy @TxIds) BlockMetaTxTypeId
        proof = BlockMetaTxWitnessProof
    in expandUnionRawTxs (\_ -> (stateTx, proof, seqExpandersBlockMetaTx)) [meta] >>= \case
           [expanded] -> return expanded
           _          -> error "expandBlockMetaTx: did not expand 1 meta into 1 sd tx"

-- | Expands block meta.
-- Should happend strictly after validation of transactions in block's body.
-- Those checks which happen in block integrity validation are missing here.
seqExpandersBlockMetaTx
    :: HasCoreConfig
    => SeqExpanders Exceptions Ids Proofs Values ctx BlockMetaTx
seqExpandersBlockMetaTx =
    SeqExpanders $ one $ Expander inP outP $ \BlockMetaTx{..} -> do
        let block@Block{ bHeader = header, bBody = body } = bmtBlock
        let issuerAddr = mkAddr (hIssuer header)
        let isGenesis = header == bHeader genesisBlock

        {- Validation -}

        when isGenesis $
            -- further we assume that our genesis block has the least possible difficulty
            unless (hDifficulty header == 0) $
                error "Genesis block difficulty must be 0"

        unless isGenesis $ do
            mSimilarHeaderHash <- queryOne (hDifficulty header)
            case mSimilarHeaderHash of
                Nothing -> pass
                Just similarHeaderHash ->
                    throwLocalError DuplicatedDifficulty
                    { bmeProvidedHeader = header, bmeExistingHeaderHash = similarHeaderHash }

            -- At this point there is no block in chain with exactly the same
            -- header as ours

            prevHash <-
                (hDifficulty header - 1)
                `assertExists` DifficultyIsTooLarge (hDifficulty header)
            (sBlockReconstruct -> Block prevHeader _) <-
                SD.BlockRef prevHash `assertExists` noPrevBlock prevHash

            unless (hPrevHash header == prevHash) $
                  throwLocalError PrevBlockIsIncorrect
                  { bmeProvidedHash = hPrevHash header, bmeTipHash = prevHash }
            unless (hSlotId header > hSlotId prevHeader) $
                  throwLocalError SlotIdIsNotIncreased
                  { bmeProvidedSlotId = hSlotId header, bmeTipSlotId = hSlotId prevHeader }

            let GovCommittee com = gcGovernance $ giveL @CoreConfig
            unless (committeeOwnsSlot com issuerAddr (hSlotId header)) $
                throwLocalError $ IssuerDoesNotOwnSlot
                { bmrSlotId = hSlotId header, bmrIssuer = issuerAddr }

        let signedData = BlockToSign (hDifficulty header) (hSlotId header)
                                     (hPrevHash header) (hash body)
        unless (verify (hIssuer header) signedData (hSignature header)) $
            throwLocalError InvalidBlockSignature

        {- Change sets -}

        let diffChange = Map.singleton
                (inj $ hDifficulty header)
                (New . BlockIdxVal $ headerHash header)

        -- no keys collision is possible because at this point hPrevHash matches tip,
        -- and block at tip is unique
        let forwardBlockChange = Map.singleton
                (inj . NextBlockOf $ hPrevHash header)
                (New . NextBlockOfVal . NextBlock $ headerHash header)

        -- no keys collision is possible sinse we have validated transactions
        -- before this function call, thus every transaction is unique
        let txBlockRefsChange = Map.fromList $
                enumerate (bbTxs (bBody block)) <&> \(idx, gTx) ->
                    ( inj . TxBlockRefId . toGTxId . unGTxWitnessed $ gTx
                    , New . TxBlockVal $ TxBlockRef (headerHash block) idx
                    )

        let totalChange = mconcat [diffChange, forwardBlockChange, txBlockRefsChange]
        pure $ mkDiffCS (totalChange :: Map.Map Ids (ValueOp Values))
  where
    -- Account prefixes are used during the computation to access current balance
    inP  = Set.fromList []
    -- Expander returns account changes only
    outP = Set.fromList [blockIdxPrefix, txBlockPrefix, nextBlockPrefix]

    noPrevBlock (h :: HeaderHash) =
        BlockMetaInternalError $ "Can't resolve previous block " +| h |+ ""
