-- | Money tx validation and expansion.
module Dscp.Snowdrop.Expanders.Account
    ( toProofBalanceTx
    , seqExpandersBalanceTx
    ) where

import Data.Coerce (coerce)
import Data.Default (Default (..))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Snowdrop.Core (Expander (..), SeqExpanders (..), StateTxType (..), ValueOp (..), mkDiffCS,
                      queryOne, queryOneExists)
import Snowdrop.Util

import Dscp.Core
import Dscp.Crypto as Dscp
import Dscp.Snowdrop.Configuration
import Dscp.Snowdrop.Expanders.Common
import Dscp.Snowdrop.Storage.Types
import Dscp.Snowdrop.Types

toProofBalanceTx :: (StateTxType, Proofs)
toProofBalanceTx = (getStateTxType AccountTxTypeId, NoProof)

seqExpandersBalanceTx :: Address
                      -> Fees
                      -> SeqExpanders Exceptions Ids Proofs Values ctx TxWitnessed
seqExpandersBalanceTx feesReceiverAddr (Fees minimalFees) =
    SeqExpanders $ one $ Expander inP outP $ \tw@TxWitnessed{..} -> do
        let txId = toTxId twTx
        let gTxId = coerce @_ @GTxId txId

        let outputs = txOuts twTx

        -- Account we transfer money from
        let txIn      = txInAcc twTx
        -- Input address
        let inAddr    = tiaAddr txIn
        -- Output matching transaction input, if any
        let outSame   = find ((== inAddr) . txOutAddr) (txOuts twTx)
        -- All outputs which does not match the input
        let outOther  = maybe outputs (`List.delete` outputs) outSame
        -- Money sent to ourselves
        let inputBack = maybe 0 (coinToInteger . txOutValue) outSame
        -- How much money user has spent as the tx input.
        let inputSent = coinToInteger $ txInValue twTx
        -- Sum of outputs
        let outputsReceived = sum $ map (coinToInteger . txOutValue) outputs
        -- Fees
        let fee       = inputSent - outputsReceived

        remChg <- expandRememberTx txId tw
        inputChg <- expandDeparture txIn inputSent (inputSent - inputBack) fee
        outputChg <- expandReceivals outputs outOther
        historyChg <- expandTxHistory gTxId (tiaAddr txIn : map txOutAddr outputs)

        verifyMoneyTxWitness tw

        let changes = toChangeMap (inputChg : remChg : outputChg ++ historyChg)

        changesWithFees <- expandAddFees fee feesReceiverAddr minimalFees changes
        pure $ mkDiffCS changesWithFees
  where
    -- Account prefixes are used during the computation to access current balance
    inP  = Set.fromList [accountPrefix, txOfPrefix]
    -- Expander returns account changes only
    outP = Set.fromList [accountPrefix, txPrefix, txHeadPrefix, txOfPrefix]

-- | Expand everything related to the transaction input.
expandDeparture :: TxInAcc -> Integer -> Integer -> Integer -> Expansion ctx ChangeEntry
expandDeparture inpAcc inputSent inputSentToOthers outputsReceived = do
    -- How much money user has sent to himself back.
    let inAddr = tiaAddr inpAcc

    account <- fromMaybe def <$> queryOne (AccountId inAddr)
    let balance = aBalance account

    -- TODO: do we ever need this check?
    -- outputs are forced to have at least 1 coin in sum
    unless (inputSent > 0) $
        throwLocalError PaymentMustBePositive

    unless (inputSent - outputsReceived >= 0) $
        throwLocalError SumMustBeNonNegative
            { aeSent = inputSent, aeReceived = outputsReceived }

    unless (balance - outputsReceived >= 0) $
        throwLocalError CannotAffordOutputs
            { aeOutputsSum = outputsReceived, aeBalance = balance }

    unless (balance - inputSent >= 0) $
        throwLocalError CannotAffordFees
            { aeSpent = inputSent, aeBalance = balance }

    unless (tiaNonce inpAcc == aNonce account) $
        throwLocalError NonceMismatch
            { aePreviousNonce = aNonce account, aeTxNonce = tiaNonce inpAcc }

    let inpNewBal     = aBalance account - inputSentToOthers
    let newInpAccount = Account
            { aBalance = inpNewBal
            , aNonce   = tiaNonce inpAcc + 1
            }

    return (AccountId inAddr ==> Upd newInpAccount)

expandReceivals
    :: [TxOut]
    -> [TxOut]
    -> Expansion ctx [ChangeEntry]
expandReceivals outputs otherOutputs = do
    -- TODO: it is weird that we validate "outputs", but "otherOutputs" participate
    -- in changeset formation.
    -- For instance, this means that if I specify a single output to myself, resulting
    -- transaction will have no outputs at all like if it was fine.
    -- Maybe outputs refering to our input should be removed at this point, or be prohibited
    -- entirely?
    when (null outputs) $
        throwLocalError MTxNoOutputs

    let uniqOutAddrs = ordNub $ map txOutAddr outputs
    when (length uniqOutAddrs /= length outputs) $
        throwLocalError MTxDuplicateOutputs

    forM otherOutputs $ \TxOut{..} -> do
        prevAcc <- queryOne (AccountId txOutAddr)
        let outVal    = coinToInteger txOutValue

        unless (outVal > 0) $
            throwLocalError OutputIsEmpty{ aeAddress = txOutAddr }

        let newAcc    = Account { aBalance = outVal,               aNonce = 0 }
        let updAcc a0 = Account { aBalance = aBalance a0 + outVal, aNonce = aNonce a0 }
        let ch        = maybe (New newAcc) (Upd . updAcc) prevAcc

        pure (AccountId txOutAddr ==> ch)

expandRememberTx :: TxId -> TxWitnessed -> Expansion ctx ChangeEntry
expandRememberTx txId tw = do
    -- before we can add next changeset entry, we have to check our transaction is new
    -- to the chain
    txAlreadyExists <- queryOneExists txId
    when txAlreadyExists $
        throwLocalError $ TransactionAlreadyExists txId

    return (txId ==> New (TxItself tw))

expandTxHistory :: GTxId -> [Address] -> Expansion ctx [ChangeEntry]
expandTxHistory gTxId txAddrs =
    fmap concat . forM txAddrs $ \txAddr ->
        queryOne (TxsOf txAddr) <&> \case
            Nothing -> [TxsOf txAddr ==> New (LastTx gTxId)]
            Just (LastTx prevTxId) ->
                [ TxsOf  txAddr       ==> Upd (LastTx gTxId)
                , TxHead txAddr gTxId ==> New (TxNext prevTxId)
                ]

expandAddFees
    :: Integer
    -> Address
    -> Coin
    -> Map Ids (ValueOp Values)
    -> Expansion ctx (Map Ids (ValueOp Values))
expandAddFees feeAmount feesReceiver minimalFees changes = do
    let feesReceiverAccountId = AccountId feesReceiver
    feesReceiverAccM <- queryOne feesReceiverAccountId
    let addFee receiver = receiver { aBalance = aBalance receiver + feeAmount }
    let onlyFees = Account { aBalance = feeAmount, aNonce = 0 }
    let feesReceiverUpd = maybe (New onlyFees) (Upd . addFee) feesReceiverAccM

    unless (feeAmount >= coinToInteger minimalFees) $
        throwLocalError InsufficientFees
            { aeActualFees = feeAmount
            , aeExpectedFees = coinToInteger minimalFees }

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

    return changesWithFees

verifyMoneyTxWitness :: TxWitnessed -> Expansion ctx ()
verifyMoneyTxWitness TxWitnessed{ twWitness = witness, twTx = tx } = do
    let txId = toTxId tx
    let pk = txwPk witness
    let input = tiaAddr $ txInAcc tx

    unless (verify pk (txId, pk) (txwSig witness)) $
        throwLocalError SignatureIsCorrupted

    unless (mkAddr pk == input) $
        throwLocalError WitnessMismatchesInput
            { aeSignerAddress = mkAddr pk, aeInput = input }
