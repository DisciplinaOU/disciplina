module Dscp.Snowdrop.Expanders
    ( expandRawBlock
    , expandGlobalTxs'
    ) where

import Control.Exception (Exception)
import Control.Lens (contramap)
import Data.Default (Default)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import qualified Snowdrop.Model.Block as SD
import Snowdrop.Model.Expander (DiffChangeSet (..), Expander (..), SeqExpanders (..),
                                expandUnionRawTxs, mkDiffCS)
import Snowdrop.Model.State.Accounting.Utxo (MoneyTx (..), TxIn (..), UtxoTxIn (..))
import Snowdrop.Model.State.Core (ChgAccum, ChgAccumCtx, ERoComp, StateTxType (..), queryOne)
import Snowdrop.Model.State.Dlg (HeavyDlgCertProof (..), HeavyDlgDelegateId (..),
                                 HeavyDlgIssuerId (..), HeavyDlgTxId (..))
import Snowdrop.Model.State.Restrict (RestrictCtx)
import Snowdrop.Util

import Dscp.Core.Types (Block (..), GlobalTx (..), StakeholderId, Tx (..), TxWithWitness (..),
                        TxWitness (..), rbbTxs)
import Dscp.Crypto (hash)
import Dscp.Snowdrop.Configuration


expandRawBlock
  :: (Default (ChgAccum ctx), HasLens ctx RestrictCtx, HasLens ctx (ChgAccumCtx ctx))
  => Block
  -> ERoComp Exceptions Ids Values ctx SBlock
expandRawBlock rb@Block{..} =
    SD.Block rbHeader <$> expandGlobalTxs' (rbbTxs rbBody)

expandGlobalTxs'
  :: (Default (ChgAccum ctx), HasLens ctx RestrictCtx, HasLens ctx (ChgAccumCtx ctx))
  => [GlobalTx]
  -> ERoComp Exceptions Ids Values ctx SPayload
expandGlobalTxs' txs = expandUnionRawTxs getByGlobalTx txs

getByGlobalTx ::
       GlobalTx
    -> ( StateTxType
       , Proofs
       , SeqExpanders Exceptions Ids Proofs Values ctx GlobalTx)
getByGlobalTx t@(MoneyTx tx) = let (a, b) = toProofBalanceTx tx in (a, b, seqExpandersGlobalTx t)

seqExpandersGlobalTx ::
       GlobalTx -> SeqExpanders Exceptions Ids Proofs Values ctx GlobalTx
seqExpandersGlobalTx (MoneyTx _) = contramap (\(MoneyTx tx) -> tx) seqExpandersBalanceTx

-- Money Tx
toProofBalanceTx :: TxWithWitness -> (StateTxType, Proofs)
toProofBalanceTx (TxWithWitness (Tx ins _) (TxWitness wit)) =
    (txType, AddressTxWitness $ M.fromList $ zip ins wit)
  where
    txType = StateTxType $ getId (Proxy @TxIds) MoneyTxId

seqExpandersBalanceTx :: SeqExpanders Exceptions Ids Proofs Values ctx TxWithWitness
seqExpandersBalanceTx =
    SeqExpanders $ one $ Expander inP outP $ \txw@TxWithWitness{..} -> do
        let txId = hash twTx
        let getCurBalance (from ::  =
        inps <- map (\txin -> (AccountInIds (UtxoTxIn txin), Rem)) (txIns twTx)
        let createTxIn = UtxoTxInIds . UtxoTxIn . TxIn txId
        let outs = zip (map createTxIn [0..]) (map (New . TxOutVal) txOuts)
        pure $ mkDiffCS $ M.fromList $ inps ++ outs
  where
    -- Account prefixes are used during the computation to access current balance
    inP = (S.singleton accountPrefix)
    -- Expander returns account changes only
    outP = (S.singleton accountPrefix)
