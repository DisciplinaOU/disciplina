{-# LANGUAGE DataKinds #-}

-- | Account-style validation. Copied from
-- 'Snowdrop.Model.State.Accounting.Account'. It'll be brought back to
-- snowdrop at some point :yao:

module Dscp.Snowdrop.AccountValidation
       ( AccountTxTypeId (..)
       , validateSimpleMoneyTransfer
       , AccountId(..)
       , Account(..)
       , Author(..)
       , AccountValidationException(..)
       -- , accountComputeFee
       , validateSaneArrival
       , validateSaneDeparture
       , checkThatAccountIsUpdatedOnly

       , authenticate
       , requirePart
       , assertSigned
       , assertExists
       , check
       ) where

import Control.Monad.Error.Class (MonadError)
import Data.List as List (partition)
import Data.Map as Map (toList)
import Dscp.Core.Foundation.Transactions (TxId)
import Dscp.Core.Types (Address)
import Dscp.Crypto (PublicKey)
import Dscp.Snowdrop.Configuration (CanVerifyPayload, Exceptions, Ids, PersonalisedProof, Proofs,
                                    TxIds, Values)
import Dscp.Snowdrop.Types (Account (..), AccountId (..), AccountTxTypeId (..),
                            AccountValidationException (..), Author (..))
import Snowdrop.Model.State.Core (ERoComp, HasKeyValue, PreValidator (..), StatePException,
                                  StateTx (..), StateTxType (..), TxValidationException (..),
                                  Validator, mkValidator, queryOne, validateIff)
import Snowdrop.Util

assertSigned
    :: ( VerifySign pk signature a
       , HasReview e e1
       , MonadError e m
       )
    => WithSignature pk signature a
    -> e1
    -> m a
assertSigned WithSignature {..} message = do
    () <- validateIff message $
        verifySignature wsPublicKey wsBody wsSignature

    return wsBody

infix 1 `check`
check :: (Monoid a, HasException e e1) => Bool -> e1 -> ERoComp e id value ctx a
check = flip validateIff

validateSimpleMoneyTransfer
    :: forall ctx
    .  CanVerifyPayload TxId ()
    => HasPrism Proofs (PersonalisedProof TxId ())
    => HasPrism Proofs TxId
    => HasGetter PublicKey Address
    => Validator Exceptions Ids Proofs Values ctx
validateSimpleMoneyTransfer = mkValidator ty
    [preValidateSimpleMoneyTransfer @ctx]
  where
    ty = StateTxType $ getId (Proxy @TxIds) AccountTxTypeId

authenticate
    :: forall txid ctx payload
    .  Eq txid
    => HasPrism Proofs (PersonalisedProof txid payload)
    => HasPrism Proofs txid
    => HasGetter PublicKey Address
    => CanVerifyPayload txid payload
    => Proofs
    -> ERoComp Exceptions Ids Values ctx (AccountId, Account, payload)
authenticate proof = do
    signedHash
        :: PersonalisedProof txid payload
        <- requirePart proof SignatureIsMissing

    realHashfromExpander <- requirePart proof TransactionIsCorrupted

    (hash, pk, payload) <- signedHash                   `assertSigned` SignatureIsCorrupted
    ()                  <- pk == wsPublicKey signedHash `check`        KeysMismatch

    let authorId = gett pk

    before <- AccountId authorId           `assertExists` AuthorDoesNotExist
    ()     <- realHashfromExpander == hash `check`        TransactionIsCorrupted

    return (AccountId authorId, before, payload)

preValidateSimpleMoneyTransfer
    :: forall       ctx
    .  CanVerifyPayload TxId ()
    => HasPrism Proofs (PersonalisedProof TxId ())
    => HasPrism Proofs TxId
    => HasGetter PublicKey Address
    => PreValidator Exceptions Ids Proofs Values ctx
preValidateSimpleMoneyTransfer =
    PreValidator $ \_trans@StateTx {..} -> do
        (author, before, ()) <- authenticate @TxId txProof

            -- Turn ChangeSet to kv-pairs.
        let updates        = Map.toList $ changeSet txBody

            -- Project both key and value to AccointId and (ValueOp Account).
            accountChanges = updates <&> \pair@ (k, _) -> (k, proj pair)

        -- Check that all pairs are projected and are Updates.
        -- Strip Update ctor from values.
        changes <- mapM checkThatAccountIsUpdatedOnly accountChanges

        let (payer, recipients) = List.partition (is author) changes

        (paid, deltas) <-
            (,) <$> validateSaneDeparture payer before
                <*> mapM validateSaneArrival recipients

        validateIff SumMustBeNonNegative $
            paid - sum deltas == 0


-- | Require that whole projects into part or throw error.
requirePart :: (HasReview e e1, MonadError e m, HasPrism s hash) => s -> e1 -> m hash
requirePart whole message = do
    case proj whole of
      Just part -> do
        return part

      Nothing -> do
        throwLocalError message

-- | Require that id exists in a database or throw error.
assertExists
    :: ( HasExceptions e [e1, StatePException]
       , Ord id
       , Ord id'
       , HasKeyValue id value id' a
       )
    => id'
    -> e1
    -> ERoComp e id value ctx a
assertExists thing message = do
    result <- queryOne thing
    case result of
      Nothing -> do
        throwLocalError message

      Just it -> do
        return it

-- TODO: Fix and unmess when fee will be actually required.
-- accountComputeFee
--   :: forall ctx
--   .  StateTx Ids Proofs Values
--   -> ERoComp Exceptions Ids Values ctx Integer
-- accountComputeFee = \StateTx{..} -> do
--     let ty = StateTxType $ getId (Proxy @AccountTxTypeId) AccountTxTypeId
--     author
--         :: Author
--         <- requirePart txProof (ProofProjectionFailed ty)
--     before <- assertExists (auAuthorId author) AuthorDoesNotExist
--     let updates = Map.toList $ changeSet txBody
--     let accountChanges = updates <&> \pair@(k, _) -> (k, proj pair)
--     changes <- mapM checkThatAccountIsUpdatedOnly accountChanges
--     let (payer, recipients) = List.partition (is (auAuthorId author)) changes
--     (paid, deltas) <-
--         (,) <$> validateSaneDeparture payer before
--             <*> mapM validateSaneArrival recipients
--     let difference = paid - sum deltas
--     () <- validateIff SumMustBeNonNegative (difference >= 0)
--     return difference

validateSaneDeparture
  :: forall ctx
  .  [(AccountId, Account)]
  -> Account
  -> ERoComp Exceptions Ids Values ctx Integer
validateSaneDeparture self before = do
    case self of
        -- Check that only one change is done for author.
        [(_, account)] -> do

            let paid = aBalance before - aBalance account
            -- Check that transaction increments author nonce.

            () <- mconcat
                [ validateIff NonceMustBeIncremented $ aNonce account == aNonce before + 1
                , validateIff PaymentMustBePositive  $ paid > 0
                ]

            return paid

        _ -> throwLocalError NotASingletonSelfUpdate

validateSaneArrival
  :: forall ctx
  .  (AccountId, Account)
  -> ERoComp Exceptions Ids Values ctx Integer
validateSaneArrival (accId, account) = do
    was <- assertExists accId ReceiverDoesNotExist
    let received = aBalance account - aBalance was
    -- Check that except for the balance the account is unchanged.
    () <- mconcat
        [ validateIff ReceiverOnlyGetsMoney       $ was { aBalance = aBalance account } == account
        , validateIff ReceiverMustIncreaseBalance $ received > 0
        ]
    return received

checkThatAccountIsUpdatedOnly
  :: forall ctx
  .  (Ids, Maybe (AccountId, ValueOp Account))
  -> ERoComp Exceptions Ids Values ctx (AccountId, Account)
checkThatAccountIsUpdatedOnly = \case
    (_, Just (accId, Upd it)) -> return (accId, it)
    (k, _)                    -> throwLocalError $ UnexpectedPayload [idSumPrefix k]

is :: AccountId -> (AccountId, Account) -> Bool
is accountId (accId, _) = accountId == accId
