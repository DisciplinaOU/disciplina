-- | Block expander.
module Dscp.Snowdrop.Expanders.Block
    ( expandBlock
    , expandGTxs
    , expandGTx
    ) where

import Control.Lens (contramap)
import Data.Default (Default (..))

import qualified Snowdrop.Block as SD
import Snowdrop.Core (ChgAccum, ChgAccumCtx, ERoComp, SeqExpanders (..), StateTxType (..))
import Snowdrop.Execution (RestrictCtx, expandUnionRawTxs)
import Snowdrop.Util

import Dscp.Core
import Dscp.Crypto as Dscp
import Dscp.Snowdrop.Configuration
import Dscp.Snowdrop.Expanders.Account
import Dscp.Snowdrop.Expanders.BlockMeta
import Dscp.Snowdrop.Expanders.Publication

-- | Expand block.
expandBlock ::
       ( Default (ChgAccum ctx)
       , HasLens ctx RestrictCtx
       , HasLens ctx (ChgAccumCtx ctx)
       , HasCoreConfig
       )
    => Bool
    -> Block
    -> ERoComp Exceptions Ids Values ctx SBlock
expandBlock applyFees block@Block{..} = do
    let issuer = hIssuer bHeader
    stateTxs <- expandGTxs applyFees issuer (bbTxs bBody)
    metaStateTx <- expandBlockMetaTx (BlockMetaTx block)
    pure $ SD.Block bHeader (SPayload (metaStateTx : stateTxs) (hash bBody))

-- | Expand list of global txs.
expandGTxs ::
       ( Default (ChgAccum ctx)
       , HasLens ctx RestrictCtx
       , HasLens ctx (ChgAccumCtx ctx)
       , HasCoreConfig
       )
    => Bool
    -> Dscp.PublicKey
    -> [GTxWitnessed]
    -> ERoComp Exceptions Ids Values ctx [SStateTx]
expandGTxs applyFees = expandUnionRawTxs . getByGTx applyFees

-- | Expand one global tx.
expandGTx ::
       ( Default (ChgAccum ctx)
       , HasLens ctx RestrictCtx
       , HasLens ctx (ChgAccumCtx ctx)
       , HasCoreConfig
       )
    => Dscp.PublicKey
    -> GTxWitnessed
    -> ERoComp Exceptions Ids Values ctx SStateTx
expandGTx pk tx =
    expandUnionRawTxs (getByGTx True pk) [tx] >>= \case
        [expanded] -> return expanded
        _          -> error "expandGTx: did not expand 1 raw tx into 1 sd tx"

getByGTx
    :: HasCoreConfig
    => Bool
    -> Dscp.PublicKey
    -> GTxWitnessed
    -> ( StateTxType
       , Proofs
       , SeqExpanders Exceptions Ids Proofs Values ctx GTxWitnessed
       )
getByGTx applyFees pk t =
    let fee  = calcFeeG (if applyFees then feeConfig else noFeesConfig) t
        addr = mkAddr pk
        (a, b) = case t of
                   GMoneyTxWitnessed tx        -> toProofBalanceTx tx
                   GPublicationTxWitnessed ptx -> toProofPublicationTx fee ptx
    in (a, b, seqExpandersGTx addr fee t)

seqExpandersGTx
    :: HasCoreConfig
    => Address
    -> Fees
    -> GTxWitnessed
    -> SeqExpanders Exceptions Ids Proofs Values ctx GTxWitnessed
seqExpandersGTx addr minFee (GMoneyTxWitnessed _) =
    flip contramap (seqExpandersBalanceTx addr minFee) $ \(GMoneyTxWitnessed tx) -> tx
seqExpandersGTx addr minFee (GPublicationTxWitnessed _) =
    flip contramap (seqExpandersPublicationTx addr minFee) $ \(GPublicationTxWitnessed ptx) -> ptx
