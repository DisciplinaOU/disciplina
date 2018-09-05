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

import Dscp.Core
import Dscp.Crypto as Dscp
import Dscp.Snowdrop.Configuration hiding (PublicationTxWitness)
import qualified Dscp.Snowdrop.Configuration as Conf (Proofs (..))
import Dscp.Snowdrop.Storage.Types
import Dscp.Snowdrop.Types
import Dscp.Util (sizeSerialised)

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
    let size = sizeSerialised t
        fee  = calculateFee (if applyFees then feeCoefficients else noFees) size
        addr = mkAddr pk
    in  case t of
            GMoneyTxWitnessed tx ->
                let (a, b) = toProofBalanceTx fee tx
                in  (a, b, seqExpandersGTx addr fee t)

            GPublicationTxWitnessed ptx ->
                let (a, b) = toProofPublicationTx fee ptx
                in  (a, b, seqExpandersGTx addr fee t)

toProofPublicationTx :: Fees -> PublicationTxWitnessed -> (StateTxType, Proofs)
toProofPublicationTx fee (PublicationTxWitnessed tx (PublicationTxWitness {..})) =
    (txType, Conf.PublicationTxWitness $ WithSignature
        { wsSignature = toDscpSig pwSig
        , wsPublicKey = toDscpPK pwPk
        , wsBody = (toPtxId tx, pwPk, Publication prev new)
        } `PersonalisedProof`
            fee
    )
  where
    PublicationTx _ new prev = tx
    txType = StateTxType $ getId (Proxy @TxIds) PublicationTxTypeId


seqExpandersPublicationTx ::
       Address
    -> Fees
    -> SeqExpanders Exceptions Ids Proofs Values ctx PublicationTxWitnessed
seqExpandersPublicationTx feesReceiverAddr fee =
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
                let fee' = fromIntegral $ coinToInteger $ unFees fee
                maybePub <- queryOne (PublicationsOf ptAuthor)
                mFeesReceiver <- queryOne (AccountId feesReceiverAddr)

                let dIssuer =
                        maybe (New Account { aBalance = fee', aNonce = 0 })
                              (\issuer -> Upd issuer { aBalance = aBalance issuer + fee' })
                              mFeesReceiver

                account@Account {..} <-
                    maybe (throwLocalError PublicationAuthorDoesNotExist) pure =<<
                    queryOne (AccountId ptAuthor)

                let feesChanges
                        | ptAuthor /= feesReceiverAddr =
                              [ AccountId ptAuthor ==>
                                    Upd account
                                        -- You can say that `PubOf ==> LastPub`
                                        -- mechanism will prevent double-pub
                                        -- but lets not depend on that impl detail.
                                        { aNonce   = aNonce + 1
                                        , aBalance = aBalance - fee'
                                        }
                              , AccountId feesReceiverAddr ==> dIssuer ]
                        -- If publication author and fees receiver are the same
                        -- address, we do not transfer any coins.
                        | otherwise = []

                let change = if isJust maybePub then Upd else New
                pure $ mkDiffCS $ Map.fromList $
                    [ PublicationsOf  ptAuthor ==> change (LastPublication ptBlock)
                    , PublicationHead ptBlock  ==> New    (PublicationNext ptPrevBlock)
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
seqExpandersGTx addr fee (GPublicationTxWitnessed _) =
    flip contramap (seqExpandersPublicationTx addr fee) $ \(GPublicationTxWitnessed ptx) -> ptx

toProofBalanceTx :: Fees -> TxWitnessed -> (StateTxType, Proofs)
toProofBalanceTx fee (TxWitnessed tx (TxWitness {..})) =
    (txType, AddressTxWitness $ WithSignature {..} `PersonalisedProof` fee)
  where
    txType = StateTxType $ getId (Proxy @TxIds) AccountTxTypeId
    wsSignature = toDscpSig txwSig
    wsPublicKey = toDscpPK txwPk
    wsBody = (toTxId tx, txwPk, ())

seqExpandersBalanceTx ::
       Address
    -> Fees
    -> SeqExpanders Exceptions Ids Proofs Values ctx TxWitnessed
seqExpandersBalanceTx feesReceiverAddr fee =
    SeqExpanders $ one $ Expander inP outP $ \TxWitnessed{..} -> do
        let feeAmount = fromIntegral $ coinToInteger $ unFees fee
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

        inpPrevAcc <- maybe (throwLocalError AuthorDoesNotExist) pure =<< getCurAcc inAddr

        let inpNewBal     = aBalance inpPrevAcc + inputBack - inputSent
        let newInpAccount = Account
                { aBalance = inpNewBal
                , aNonce   = fromIntegral (tiaNonce $ txInAcc twTx) + 1
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
            ExpanderInternalError "Changeset produced by tx has duplicates"

        -- Adding fees
        let feesReceiverAccountId = AccountId feesReceiverAddr
        feesReceiverAccM <- queryOne feesReceiverAccountId
        let addFee receiver = receiver { aBalance = aBalance receiver + feeAmount }
        let onlyFees = Account { aBalance = feeAmount, aNonce = 0 }
        let feesReceiverUpd = maybe (New onlyFees) (Upd . addFee) feesReceiverAccM

        let changes :: Map Ids (ValueOp Values)
            changes = Map.fromList changesList
        let changesWithFees =
                -- Current key we're interested in
                let k = inj feesReceiverAccountId
                    updated =
                        Map.adjust (fmap (\(AccountOutVal x) -> AccountOutVal $ addFee x)) k changes
                in case Map.lookup k changes of
                    Nothing      -> Map.insert k (inj feesReceiverUpd) changes
                    Just (New _) -> updated
                    Just (Upd _) -> updated
                    Just Rem     -> Map.insert k (New $ inj onlyFees) changes
                    other        -> error $ "changesWithFese: not expected: " <> show other

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
