module Dscp.Snowdrop.Expanders
    ( expandBlock
    , expandGTxs
    , expandGTx
    ) where

import Control.Lens (contramap)
import Data.Default (Default)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Snowdrop.Block as SD
import Snowdrop.Core (ChgAccum, ChgAccumCtx, ERoComp, Expander (..), SeqExpanders (..),
                      StateTxType (..), ValueOp (..), mkDiffCS, queryOne)
import Snowdrop.Execution (RestrictCtx, expandUnionRawTxs)
import Snowdrop.Util

import Dscp.Core.Foundation
import Dscp.Crypto (hash, toDscpPK, toDscpSig)
import Dscp.Snowdrop.Configuration hiding (PublicationTxWitness)
import qualified Dscp.Snowdrop.Configuration as Conf (Proofs (..))
import Dscp.Snowdrop.Storage.Types
import Dscp.Snowdrop.Types

----------------------------------------------------------------------------
-- Top-level expanders
----------------------------------------------------------------------------

-- | Expand block.
expandBlock ::
       ( Default (ChgAccum ctx)
       , HasLens ctx RestrictCtx
       , HasLens ctx (ChgAccumCtx ctx)
       )
    => Block
    -> ERoComp Exceptions Ids Values ctx SBlock
expandBlock Block{..} = do
    stateTxs <- expandGTxs (bbTxs bBody)
    pure $ SD.Block bHeader (SPayload stateTxs $ hash bBody)

-- | Expand list of global txs.
expandGTxs ::
       ( Default (ChgAccum ctx)
       , HasLens ctx RestrictCtx
       , HasLens ctx (ChgAccumCtx ctx)
       )
    => [GTxWitnessed]
    -> ERoComp Exceptions Ids Values ctx [SStateTx]
expandGTxs txs = expandUnionRawTxs getByGTx txs

-- | Expand list of global txs.
expandGTx ::
       ( Default (ChgAccum ctx)
       , HasLens ctx RestrictCtx
       , HasLens ctx (ChgAccumCtx ctx)
       )
    => GTxWitnessed
    -> ERoComp Exceptions Ids Values ctx SStateTx
expandGTx txs =
    expandUnionRawTxs getByGTx [txs] >>= \case
        [expanded] -> return expanded
        _          -> error "expandGTx: did not expand 1 raw tx into 1 sd tx"

getByGTx ::
       GTxWitnessed
    -> ( StateTxType
       , Proofs
       , SeqExpanders Exceptions Ids Proofs Values ctx GTxWitnessed)
getByGTx t = case t of
    GMoneyTxWitnessed tx ->
        let (a, b) = toProofBalanceTx tx
        in  (a, b, seqExpandersGTx t)

    GPublicationTxWitnessed ptx ->
        let (a, b) = toProofPublicationTx ptx
        in  (a, b, seqExpandersGTx t)

toProofPublicationTx :: PublicationTxWitnessed -> (StateTxType, Proofs)
toProofPublicationTx (PublicationTxWitnessed tx (PublicationTxWitness {..})) =
    (txType, Conf.PublicationTxWitness $ WithSignature
        { wsSignature = toDscpSig pwSig
        , wsPublicKey = toDscpPK pwPk
        , wsBody = (toPtxId tx, pwPk, Publication prev new)
        })
  where
    PublicationTx _ new prev = tx
    txType = StateTxType $ getId (Proxy @TxIds) PublicationTxTypeId

seqExpandersPublicationTx :: SeqExpanders Exceptions Ids Proofs Values ctx PublicationTxWitnessed
seqExpandersPublicationTx =
    SeqExpanders $ one $ Expander
        (Set.fromList [publicationOfPrefix])
        (Set.fromList [accountPrefix, publicationHeadPrefix, publicationOfPrefix])
        $ \PublicationTxWitnessed
            { ptwTx = PublicationTx
                { ptAuthor
                , ptPrevBlock
                , ptBlock
                }
            } -> do
                maybePub <- queryOne (PublicationsOf ptAuthor)

                let change = if isJust maybePub then Upd else New

                return $ mkDiffCS $ Map.fromList
                  [ PublicationsOf  ptAuthor ==> change (LastPublication ptBlock)
                  , PublicationHead ptBlock  ==> New    (PublicationNext ptPrevBlock)
                  ]

----------------------------------------------------------------------------
-- Money tx
----------------------------------------------------------------------------

seqExpandersGTx ::
       GTxWitnessed -> SeqExpanders Exceptions Ids Proofs Values ctx GTxWitnessed
seqExpandersGTx (GMoneyTxWitnessed _) =
    contramap (\(GMoneyTxWitnessed tx) -> tx) seqExpandersBalanceTx
seqExpandersGTx (GPublicationTxWitnessed _) =
    contramap (\(GPublicationTxWitnessed ptx) -> ptx) seqExpandersPublicationTx

toProofBalanceTx :: TxWitnessed -> (StateTxType, Proofs)
toProofBalanceTx (TxWitnessed tx (TxWitness {..})) =
    (txType, AddressTxWitness $ WithSignature {..})
  where
    txType = StateTxType $ getId (Proxy @TxIds) AccountTxTypeId
    wsSignature = toDscpSig txwSig
    wsPublicKey = toDscpPK txwPk
    wsBody = (toTxId tx, txwPk, ())

seqExpandersBalanceTx :: SeqExpanders Exceptions Ids Proofs Values ctx TxWitnessed
seqExpandersBalanceTx =
    SeqExpanders $ one $ Expander inP outP $ \TxWitnessed{..} -> do
        let outputs = txOuts twTx
        -- check for output duplicates
        let uniqOutAddrs = ordNub $ map txOutAddr outputs

        when (length outputs > 1 && length uniqOutAddrs /= length outputs) $
            throwLocalError MTxDuplicateOutputs

        -- Account we transfer money from
        let inAddr    = tiaAddr $ txInAcc twTx
        let getCurAcc = queryOne @_ @AccountId @_ @Account . AccountId
        let outSame   = find ((== inAddr) . txOutAddr) (txOuts twTx)
        let outOther  = maybe outputs (`List.delete` outputs) outSame

        -- How much money user has sent to himself back.
        let inputBack = maybe 0 (coinToInteger . txOutValue) outSame
        -- How much money user has spent as the tx input.
        let inputSent = coinToInteger $ txInValue twTx

        inpPrevAcc <- maybe (throwLocalError CantResolveSender) pure =<< getCurAcc inAddr

        let inpNewBal     = aBalance inpPrevAcc + inputBack - inputSent
        let newInpAccount = Account
                { aBalance = inpNewBal
                , aNonce   = tiaNonce (txInAcc twTx)
                }

        let inp = AccountId inAddr ==> Upd newInpAccount

        outs <- forM outOther $ \TxOut{..} -> do
            prevAcc <- getCurAcc txOutAddr

            let outVal    = coinToInteger txOutValue
            let newAcc    = Account { aBalance = outVal,               aNonce = 0 }
            let updAcc a0 = Account { aBalance = aBalance a0 + outVal, aNonce = aNonce a0 }
            let ch        = maybe (New newAcc) (Upd . updAcc) prevAcc

            pure (AccountId txOutAddr ==> ch)

        let txId = toGTxId $ GMoneyTx twTx

        accList <- forM (inAddr : map txOutAddr outOther) $ \addr ->
            queryOne (TxsOf addr) >>= return . \case
                Nothing -> [TxsOf addr ==> New (LastTx txId)]
                Just (LastTx prevTxId) ->
                    [ TxsOf  addr      ==> Upd (LastTx txId)
                    , TxHead addr txId ==> New (TxNext prevTxId)
                    ]

        pure $ mkDiffCS $ Map.fromList $ inp : outs ++ concat accList
  where
    -- Account prefixes are used during the computation to access current balance
    inP  = Set.fromList [accountPrefix, txOfPrefix]
    -- Expander returns account changes only
    outP = Set.fromList [accountPrefix, txHeadPrefix, txOfPrefix]

--------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------

(==>)
    :: forall id value k v
    .  HasReview k id
    => HasReview v value
    => id
    -> value
    -> (k, v)
k ==> v = (inj k, inj v)
