{-# LANGUAGE DataKinds #-}

-- | Account-style validation. Copied from
-- 'Snowdrop.Model.State.Accounting.Account'. It'll be brought back to
-- snowdrop at some point :yao:

module Dscp.Snowdrop.AccountValidation
       ( validateSimpleMoneyTransfer

       , Authenticated (..)
       , authenticate
       , requirePart
       , assertSigned
       , assertExists
       , nothingToLocalError
       , check
       ) where

import qualified Data.List as List
import qualified Data.Map as Map
import Snowdrop.Core (ERoComp, PreValidator (..), StateTx (..),
                      StateTxType (..), TxValidationException (..), Validator, ValueOp (..),
                      changeSet, idSumPrefix, mkValidator, queryOne)
import Snowdrop.Util

import Dscp.Core
import qualified Dscp.Crypto as DC (PublicKey)
import Dscp.Snowdrop.Authentication
import Dscp.Snowdrop.Assertions
import Dscp.Snowdrop.Configuration (CanVerifyPayload, Exceptions, Ids, PersonalisedProof (..),
                                    Proofs, TxIds, Values, accountPrefix)
import Dscp.Snowdrop.Types

validateSimpleMoneyTransfer ::
       forall ctx.
       ( CanVerifyPayload TxId ()
       , HasPrism Proofs (PersonalisedProof TxId ())
       , HasPrism Proofs TxId
       , HasGetter DC.PublicKey Address
       )
    => Validator Exceptions Ids Proofs Values ctx
validateSimpleMoneyTransfer = mkValidator ty
    [preValidateSimpleMoneyTransfer @ctx]
  where
    ty = StateTxType $ getId (Proxy @TxIds) AccountTxTypeId

preValidateSimpleMoneyTransfer ::
       forall ctx.
       ( CanVerifyPayload TxId ()
       , HasPrism Proofs (PersonalisedProof TxId ())
       , HasPrism Proofs TxId
       , HasGetter DC.PublicKey Address
       )
    => PreValidator Exceptions Ids Proofs Values ctx
preValidateSimpleMoneyTransfer =
    PreValidator $ \_trans@StateTx {..} -> do
        Authenticated author before () fees <- authenticate @TxId txProof

            -- Turn ChangeSet to kv-pairs.
        let updates        = Map.toList $ changeSet txBody

            -- Filter out non-account updates
            accountUpdates = filter ((==) accountPrefix . idSumPrefix . fst) updates

            -- Project both key and value to AccointId and (ValueOp Account).
            accountChanges = accountUpdates <&> \pair@ (k, _) -> (k, proj pair)

        -- Check that all pairs are projected and are Updates.
        -- Strip Update ctor from values.
        changes <- mapM checkThatAccountIsUpdatedOnly accountChanges

        let isAuthor :: (AccountId, Account) -> Bool
            isAuthor (accId, _) = accId == author
        let (payer, recipients) = List.partition isAuthor changes

        allOutputs <- mapM validateSaneArrival recipients

        let fees' = coinToInteger (unFees fees)
        let balanceBefore = aBalance before
        -- Total amount of coins sent (sum of real tx outputs + a fee output)
        let receivedTotal = sum allOutputs
        -- Previous value minus fees (the outputs that were specified in the
        -- tx itself + innacuracy in fee calcuation comparing to minimal fees)
        let receivedNoFees = receivedTotal - fees'

        paid <- validateSaneDeparture payer before

        unless (paid >= receivedTotal) $
            throwLocalError SumMustBeNonNegative
                { aeSent = paid, aeReceived = receivedNoFees
                , aeFees = coinToInteger (unFees fees) }

        unless (balanceBefore - receivedNoFees >= 0) $
            throwLocalError BalanceCannotBecomeNegative
                { aeSpent = paid, aeBalance = balanceBefore }

        unless (balanceBefore - receivedTotal >= 0) $
            throwLocalError CannotAffordFees
                { aeOutputsSum = receivedNoFees, aeBalance = balanceBefore
                , aeFees = fees' }

validateSaneDeparture
  :: forall ctx
  .  [(AccountId, Account)]
  -> Account
  -> ERoComp Exceptions Ids Values ctx Integer
validateSaneDeparture self before = case self of
    -- Check that only one change is done for author.
    [(AccountId _, account)] -> do
        let paid = aBalance before - aBalance account
        -- Check that transaction increments author nonce.

        unless (aNonce account == aNonce before + 1) $
            throwLocalError NonceMustBeIncremented
                { aePreviousNonce = aNonce before, aeNewNonce = aNonce account }
        unless (paid > 0) $
            throwLocalError PaymentMustBePositive
        unless (aBalance account >= 0) $
            throwLocalError BalanceCannotBecomeNegative
                { aeSpent = paid, aeBalance = aBalance before }

        return paid

    _ -> throwLocalError NotASingletonSelfUpdate

validateSaneArrival
  :: forall ctx
  .  (AccountId, Account)
  -> ERoComp Exceptions Ids Values ctx Integer
validateSaneArrival (accId, account) = do
    was <- queryOne accId
    let received = aBalance account - maybe 0 aBalance was
    -- Check that except for the balance the account is unchanged.
    unless (maybe account (\w -> w{ aBalance = aBalance account }) was == account) $
        throwLocalError ReceiverOnlyGetsMoney
    unless (received > 0) $
        throwLocalError ReceiverMustIncreaseBalance
    return received

checkThatAccountIsUpdatedOnly
  :: forall ctx
  .  (Ids, Maybe (AccountId, ValueOp Account))
  -> ERoComp Exceptions Ids Values ctx (AccountId, Account)
checkThatAccountIsUpdatedOnly = \case
    (_, Just (accId, Upd it)) -> return (accId, it)
    (_, Just (accId, New it)) -> return (accId, it)
    (k, _)                    -> throwLocalError $ UnexpectedPayload [idSumPrefix k]
