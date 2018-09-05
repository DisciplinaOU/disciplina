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
       , check
       ) where

import Control.Monad.Error.Class (MonadError)
import Data.List as List (partition)
import Data.Map as Map (toList)
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

validateSimpleMoneyTransfer
    :: forall ctx
    .  CanVerifyPayload TxId ()
    => HasPrism Proofs (PersonalisedProof TxId ())
    => HasPrism Proofs TxId
    => HasGetter DC.PublicKey Address
    => Validator Exceptions Ids Proofs Values ctx
validateSimpleMoneyTransfer = mkValidator ty
    [preValidateSimpleMoneyTransfer @ctx]
  where
    ty = StateTxType $ getId (Proxy @TxIds) AccountTxTypeId

data Authenticated payload = Authenticated
    { aAccountId :: AccountId
    , aAccount   :: Account
    , aPayload   :: payload
    , aFees      :: Fees
    } deriving (Show)

authenticate
    :: forall txid ctx payload
    .  Eq txid
    => HasPrism Proofs (PersonalisedProof txid payload)
    => HasPrism Proofs txid
    => HasGetter DC.PublicKey Address
    => CanVerifyPayload txid payload
    => Proofs
    -> ERoComp Exceptions Ids Values ctx (Authenticated payload)
authenticate proof = do
    PersonalisedProof signedHash fees
        :: PersonalisedProof txid payload
        <- requirePart proof SignatureIsMissing

    realHashfromExpander <- requirePart proof TransactionIsCorrupted

    (hash, pk, payload) <- signedHash `assertSigned` SignatureIsCorrupted

    let authorId = gett pk

    before <- AccountId authorId           `assertExists` AuthorDoesNotExist
    ()     <- realHashfromExpander == hash `check`        TransactionIsCorrupted

    return $ Authenticated
        (AccountId authorId)
        before
        payload
        fees

preValidateSimpleMoneyTransfer
    :: forall       ctx
    .  CanVerifyPayload TxId ()
    => HasPrism Proofs (PersonalisedProof TxId ())
    => HasPrism Proofs TxId
    => HasGetter DC.PublicKey Address
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

        let balanceBefore = aBalance before
        -- Total amount of coins sent (sum of outputs)
        let receivedTotal = sum allOutputs
        -- Previous value minus fees
        let receivedNoFees = receivedTotal - coinToInteger (unFees fees)

        unless (balanceBefore - receivedNoFees >= 0) $ throwLocalError SumMustBeNonNegative
        unless (balanceBefore - receivedTotal >= 0) $ throwLocalError CannotAffordFees

        validateSaneDeparture payer before

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

validateSaneDeparture
  :: forall ctx
  .  [(AccountId, Account)]
  -> Account
  -> ERoComp Exceptions Ids Values ctx ()
validateSaneDeparture self before = case self of
    -- Check that only one change is done for author.
    [(AccountId _, account)] -> do
        let paid = aBalance before - aBalance account
        -- Check that transaction increments author nonce.

        () <- mconcat
            [ validateIff NonceMustBeIncremented      $ aNonce account == aNonce before + 1
            , validateIff PaymentMustBePositive       $ paid > 0
            , validateIff BalanceCannotBecomeNegative $ aBalance account >= 0
            ]
        pass
    _ -> throwLocalError NotASingletonSelfUpdate

validateSaneArrival
  :: forall ctx
  .  (AccountId, Account)
  -> ERoComp Exceptions Ids Values ctx Integer
validateSaneArrival (accId, account) = do
    was <- queryOne accId
    let received = aBalance account - maybe 0 aBalance was
    -- Check that except for the balance the account is unchanged.
    () <- mconcat
        [ validateIff ReceiverOnlyGetsMoney       $ maybe account (\w -> w{ aBalance = aBalance account }) was == account
        , validateIff ReceiverMustIncreaseBalance $ received > 0
        ]
    return received

checkThatAccountIsUpdatedOnly
  :: forall ctx
  .  (Ids, Maybe (AccountId, ValueOp Account))
  -> ERoComp Exceptions Ids Values ctx (AccountId, Account)
checkThatAccountIsUpdatedOnly = \case
    (_, Just (accId, Upd it)) -> return (accId, it)
    (_, Just (accId, New it)) -> return (accId, it)
    (k, _)                    -> throwLocalError $ UnexpectedPayload [idSumPrefix k]
