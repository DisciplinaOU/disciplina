module Dscp.Snowdrop.Expanders
    ( expandBlock
    , expandGTxs
    ) where

import Control.Lens (contramap)
import Data.Default (Default)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import qualified Snowdrop.Model.Block as SD
import Snowdrop.Model.Expander (Expander (..), SeqExpanders (..), expandUnionRawTxs, mkDiffCS)
import Snowdrop.Model.State.Core (ChgAccum, ChgAccumCtx, ERoComp, StateTxType (..), queryOne)
import Snowdrop.Model.State.Restrict (RestrictCtx)
import Snowdrop.Util

import Dscp.Core
import Dscp.Snowdrop.AccountValidation (Account (..), AccountId (..), AccountTx (..))
import Dscp.Snowdrop.Configuration

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
expandBlock Block{..} = SD.Block rbHeader <$> expandGTxs (rbbTxs rbBody)

-- | Expand list of global txs.
expandGTxs ::
       ( Default (ChgAccum ctx)
       , HasLens ctx RestrictCtx
       , HasLens ctx (ChgAccumCtx ctx)
       )
    => [GTxWitnessed]
    -> ERoComp Exceptions Ids Values ctx SPayload
expandGTxs txs = expandUnionRawTxs getByGTx txs

getByGTx ::
       GTxWitnessed
    -> ( StateTxType
       , Proofs
       , SeqExpanders Exceptions Ids Proofs Values ctx GTxWitnessed)
getByGTx t@(GMoneyTxWitnessed tx) =
    let (a, b) = toProofBalanceTx tx in (a, b, seqExpandersGTx t)

seqExpandersGTx ::
       GTxWitnessed -> SeqExpanders Exceptions Ids Proofs Values ctx GTxWitnessed
seqExpandersGTx (GMoneyTxWitnessed _) =
    contramap (\(GMoneyTxWitnessed tx) -> tx) seqExpandersBalanceTx

----------------------------------------------------------------------------
-- Money tx
----------------------------------------------------------------------------

toProofBalanceTx :: TxWitnessed -> (StateTxType, Proofs)
toProofBalanceTx (TxWitnessed tx (TxWitness {..})) =
    (txType, AddressTxWitness $ WithSignature {..})
  where
    txType = StateTxType $ getId (Proxy @TxIds) AccountTxId
    wsSignature = txwSig
    wsPublicKey = txwPk
    wsBody = (txId tx, txInAcc tx)

seqExpandersBalanceTx :: SeqExpanders Exceptions Ids Proofs Values ctx TxWitnessed
seqExpandersBalanceTx =
    SeqExpanders $ one $ Expander inP outP $ \TxWitnessed{..} -> do
        let outputs = txOuts twTx
        -- check for output duplicates
        let uniqOutAddrs = ordNub $ map txOutAddr outputs
        when (length outputs > 1 && length uniqOutAddrs /= length outputs) $
            throwLocalError MTxDuplicateOutputs

        -- Account we transfer money from
        let inAddr = tiaAddr $ txInAcc twTx
        let getCurAcc (fromAddr :: Address) = do
                (acc :: Maybe Account) <- queryOne (AccountId fromAddr)
                pure acc
        let outSame = find ((== inAddr) . txOutAddr) (txOuts twTx)
        let outOther = maybe outputs (`L.delete` outputs) outSame

        -- How much money user has sent to himself back.
        let inputBack :: Integer
            inputBack = maybe 0 (coinToInteger . txOutValue) outSame
        -- How much money user has spent as the tx input.
        let inputSent :: Integer
            inputSent = coinToInteger $ txInValue twTx

        inpPrevAcc <- maybe (throwLocalError CantResolveSender) pure =<< getCurAcc inAddr
        let inpNewBal = aBalance inpPrevAcc + inputBack - inputSent
        let newInpAccount = Account { aBalance = inpNewBal
                                    , aNonce = tiaNonce (txInAcc twTx) + 1 }
        let inp = (AccountInIds (AccountId inAddr), Upd $ AccountOutVal newInpAccount)

        outs <- forM outOther $ \TxOut{..} -> do
            prevAcc <- getCurAcc txOutAddr
            let outVal = coinToInteger txOutValue
            let newAcc = Account { aBalance = outVal , aNonce = 0 }
            let updAcc a0 = Account { aBalance = aBalance a0 + outVal, aNonce = aNonce a0 }
            let ch = maybe (New $ AccountOutVal newAcc) (Upd . AccountOutVal . updAcc) prevAcc
            pure (AccountInIds (AccountId txOutAddr), ch)

        pure $ mkDiffCS $ M.fromList $ inp : outs
  where
    -- Account prefixes are used during the computation to access current balance
    inP = (S.singleton accountPrefix)
    -- Expander returns account changes only
    outP = (S.singleton accountPrefix)
