module Dscp.Snowdrop.Expanders
    ( expandBlock
    , expandGTxs
    , expandGTx
    ) where

import Control.Lens (contramap)
import Data.Default (Default (..))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Snowdrop.Block as SD
import Snowdrop.Core (ChgAccum, ChgAccumCtx, ERoComp, Expander (..), SeqExpanders (..),
                      StateTxType (..), ValueOp (..), mkDiffCS, queryOne)
import Snowdrop.Execution (RestrictCtx, expandUnionRawTxs)
import Snowdrop.Util

import Dscp.Core
import Dscp.Crypto as Dscp
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
       , HasCoreConfig
       )
    => Bool
    -> Block
    -> ERoComp Exceptions Ids Values ctx SBlock
expandBlock applyFees Block{..} = do
    let issuer = hIssuer bHeader
    stateTxs <- expandGTxs applyFees issuer (bbTxs bBody)
    pure $ SD.Block bHeader (SPayload stateTxs $ hash bBody)

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
                   GMoneyTxWitnessed tx        -> toProofBalanceTx fee tx
                   GPublicationTxWitnessed ptx -> toProofPublicationTx fee ptx
    in (a, b, seqExpandersGTx addr fee t)

----------------------------------------------------------------------------
-- Publication tx
----------------------------------------------------------------------------

toProofPublicationTx :: Fees -> PublicationTxWitnessed -> (StateTxType, Proofs)
toProofPublicationTx fee (PublicationTxWitnessed tx (PublicationTxWitness {..})) =
    (txType, Conf.PublicationTxWitness $ WithSignature
        { wsSignature = toDscpSig pwSig
        , wsPublicKey = toDscpPK pwPk
        , wsBody = (toPtxId tx, pwPk, ptHeader)
        } `PersonalisedProof`
            fee
    )
  where
    PublicationTx { ptHeader } = tx
    txType = StateTxType $ getId (Proxy @TxIds) PublicationTxTypeId


seqExpandersPublicationTx ::
       Address
    -> SeqExpanders Exceptions Ids Proofs Values ctx PublicationTxWitnessed
seqExpandersPublicationTx feesReceiverAddr =
    SeqExpanders $ one $ Expander
        (Set.fromList [publicationOfPrefix])
        (Set.fromList [accountPrefix, publicationHeadPrefix, publicationOfPrefix,
                       publicationIdsPrefix, publicationBlockIdsPrefix, privateBlockTxPrefix])
        $ \PublicationTxWitnessed { ptwTx } -> do
            let PublicationTx{ ptHeader, ptAuthor, ptFeesAmount } = ptwTx
            let ptxId = hash ptwTx
            let phHash = hash ptHeader
            let prevHash = ptHeader ^. pbhPrevBlock
            let (prevHashM :: Maybe PrivateHeaderHash) =
                    prevHash <$ guard (prevHash /= genesisHeaderHash)

            when (prevHash == phHash) $
                throwLocalError PublicationLocalLoop

            let feeAmount = fromIntegral $ coinToInteger ptFeesAmount
            maybePub <- queryOne (PublicationsOf ptAuthor)
            mFeesReceiver <- queryOne (AccountId feesReceiverAddr)

            let dIssuer =
                    maybe (New Account { aBalance = feeAmount, aNonce = 0 })
                          (\issuer -> Upd issuer { aBalance = aBalance issuer + feeAmount })
                          mFeesReceiver

            account@Account {..} <- fromMaybe def <$> queryOne (AccountId ptAuthor)

            let feesChanges
                    | feeAmount == 0 = []
                    | ptAuthor /= feesReceiverAddr =
                          [ AccountId ptAuthor ==>
                                Upd account
                                    -- You can say that `PubOf ==> LastPub`
                                    -- mechanism will prevent double-pub
                                    -- but lets not depend on that impl detail.
                                    { aNonce   = aNonce + 1
                                    , aBalance = aBalance - feeAmount
                                    }
                          , AccountId feesReceiverAddr ==> dIssuer ]
                    -- If publication author and fees receiver are the same
                    -- address, we do not transfer any coins.
                    | otherwise = []

            let change = if isJust maybePub then Upd else New
            pure $ mkDiffCS $ Map.fromList $
                [ PublicationsOf  ptAuthor ==> change (LastPublication phHash)
                , PublicationHead phHash   ==> New    (PublicationNext prevHashM)
                , PublicationIds  ptxId    ==> New    (PublicationData ptwTx (hash ptwTx))
                , PrivateBlockTx  phHash   ==> New    (PrivateBlockTxVal ptxId)
                ] ++ feesChanges

----------------------------------------------------------------------------
-- Money tx
----------------------------------------------------------------------------

seqExpandersGTx
    :: Address
    -> Fees
    -> GTxWitnessed
    -> SeqExpanders Exceptions Ids Proofs Values ctx GTxWitnessed
seqExpandersGTx addr fee (GMoneyTxWitnessed _) =
    flip contramap (seqExpandersBalanceTx addr fee) $ \(GMoneyTxWitnessed tx) -> tx
seqExpandersGTx addr _ (GPublicationTxWitnessed _) =
    flip contramap (seqExpandersPublicationTx addr) $ \(GPublicationTxWitnessed ptx) -> ptx

toProofBalanceTx :: Fees -> TxWitnessed -> (StateTxType, Proofs)
toProofBalanceTx fee (TxWitnessed tx (TxWitness {..})) =
    (txType, AddressTxWitness $ WithSignature {..} `PersonalisedProof` fee)
  where
    txType = StateTxType $ getId (Proxy @TxIds) AccountTxTypeId
    wsSignature = toDscpSig txwSig
    wsPublicKey = toDscpPK txwPk
    wsBody = (toTxId tx, txwPk, ())

seqExpandersBalanceTx :: Address
                      -> Fees
                      -> SeqExpanders Exceptions Ids Proofs Values ctx TxWitnessed
seqExpandersBalanceTx feesReceiverAddr (Fees minimalFees) =
    SeqExpanders $ one $ Expander inP outP $ \TxWitnessed{..} -> do
        let outputs = txOuts twTx
        -- check for output duplicates
        let uniqOutAddrs = ordNub $ map txOutAddr outputs

        when (null outputs) $
            throwLocalError MTxNoOutputs

        when (length uniqOutAddrs /= length outputs) $
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

        inpPrevAcc <- fromMaybe def <$> getCurAcc inAddr

        let inpNewBal     = aBalance inpPrevAcc + inputBack - inputSent
        let newInpAccount = Account
                { aBalance = inpNewBal
                , aNonce   = tiaNonce (txInAcc twTx) + 1
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
        miscChanges <- forM (inAddr : map txOutAddr outOther) $ \txAddr ->
            queryOne (TxsOf txAddr) >>= return . \case
                Nothing -> [TxsOf txAddr ==> New (LastTx txId)]
                Just (LastTx prevTxId) ->
                    [ TxsOf  txAddr      ==> Upd (LastTx txId)
                    , TxHead txAddr txId ==> New (TxNext prevTxId)
                    ]

        -- Money flow related changes (w/o fees)
        let changesList = inp : outs ++ concat miscChanges

        -- Checking for duplicates
        let hasDuplicates l = length l /= length (ordNub l)
        when (hasDuplicates $ map fst changesList) $
            throwLocalError $
            AccountInternalError "Changeset produced by tx has duplicates"

        -- Adding fees
        let feeAmount =
                max 0 $ inputSent - sum (map (coinToInteger . txOutValue) outputs)
        let feesReceiverAccountId = AccountId feesReceiverAddr
        feesReceiverAccM <- queryOne feesReceiverAccountId
        let addFee receiver = receiver { aBalance = aBalance receiver + feeAmount }
        let onlyFees = Account { aBalance = feeAmount, aNonce = 0 }
        let feesReceiverUpd = maybe (New onlyFees) (Upd . addFee) feesReceiverAccM

        unless (feeAmount >= coinToInteger minimalFees) $
            throwLocalError InsufficientFees
                { aeActualFees = feeAmount
                , aeExpectedFees = coinToInteger minimalFees }

        let changes :: Map Ids (ValueOp Values)
            changes = Map.fromList changesList
        let changesWithFees
                | feeAmount == 0 = changes
                | otherwise =
                -- Current key we're interested in
                let k = inj feesReceiverAccountId
                    updated =
                        Map.adjust (fmap (\(AccountOutVal x) -> AccountOutVal $ addFee x)) k changes
                in case Map.lookup k changes of
                    Nothing      -> Map.insert k (inj feesReceiverUpd) changes
                    Just (New _) -> updated
                    Just (Upd _) -> updated
                    Just Rem     -> Map.insert k (New $ inj onlyFees) changes
                    other        -> error $ "changesWithFees: not expected: " <> show other

        pure $ mkDiffCS changesWithFees
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
