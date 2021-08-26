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

import Control.Monad.Error.Class (MonadError)
import Data.Default (def)
import qualified Data.List as List
import qualified Data.Map as Map
import Snowdrop.Core (ERoComp, HasKeyValue, PreValidator (..), StatePException, StateTx (..),
                      StateTxType (..), TxValidationException (..), Validator, ValueOp (..),
                      changeSet, idSumPrefix, mkValidator, queryOne, validateIff)
import Snowdrop.Util

import Dscp.Core
import qualified Dscp.Crypto as DC (PublicKey)
import Dscp.Snowdrop.Configuration (CanVerifyPayload, Exceptions, Ids, PersonalisedProof (..),
                                    Proofs, TxIds, Values, accountPrefix)
import Dscp.Snowdrop.Types

assertSigned
    :: ( VerifySign sigScheme a
       , HasReview e e1
       , MonadError e m
       )
    => WithSignature sigScheme a
    -> e1
    -> m a
assertSigned WithSignature {..} message = do
    () <- validateIff message $
        verifySignature wsPublicKey wsBody wsSignature

    return wsBody

infix 1 `check`
check :: (Monoid a, HasException e e1) => Bool -> e1 -> ERoComp e id value ctx a
check = flip validateIff

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

data Authenticated payload = Authenticated
    { aAccountId :: AccountId
    , aAccount   :: Account
    , aPayload   :: payload
    , aMinFees   :: Fees -- ^ The minimum fees tx sender must pay.
    } deriving (Show)

authenticate ::
       forall txid ctx payload.
       ( Eq txid
       , HasPrism Proofs (PersonalisedProof txid payload)
       , HasPrism Proofs txid
       , HasGetter DC.PublicKey Address
       , CanVerifyPayload txid payload
       )
    => Proofs
    -> ERoComp Exceptions Ids Values ctx (Authenticated payload)
authenticate proof = do
    PersonalisedProof signedHash fees
        :: PersonalisedProof txid payload
        <- requirePart proof SignatureIsMissing

    realHashfromExpander <- requirePart proof TransactionIsCorrupted

    (hash, pk, payload) <- signedHash `assertSigned` SignatureIsCorrupted

    let authorId = gett pk

    before <- fromMaybe def <$> queryOne (AccountId authorId)
    ()     <- realHashfromExpander == hash `check` TransactionIsCorrupted

    return $ Authenticated
        (AccountId authorId)
        before
        payload
        fees

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

-- | Require that whole projects into part or throw error.
requirePart :: (HasReview e e1, MonadError e m, HasPrism s hash) => s -> e1 -> m hash
requirePart whole message = maybe (throwLocalError message) pure $ proj whole

-- | Require that id exists in a database or throw error.
assertExists ::
       ( HasExceptions e '[ e1, StatePException]
       , Ord id
       , Ord id'
       , HasKeyValue id value id' a
       )
    => id'
    -> e1
    -> ERoComp e id value ctx a
assertExists thing message =
    maybe (throwLocalError message) pure =<< queryOne thing

nothingToLocalError
    :: HasReview e e1
    => e1 -> Maybe a -> ERoComp e id value ctx a
nothingToLocalError err mThing = maybe (throwLocalError err) pure mThing

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
