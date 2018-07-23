{-# LANGUAGE DataKinds #-}

-- | Account-style validation. Copied from
-- 'Snowdrop.Model.State.Accounting.Account'. It'll be brought back to
-- snowdrop at some point :yao:

module Dscp.Snowdrop.AccountValidation
       ( AccountTx (..)
       , validateSimpleMoneyTransfer
       , AccountId(..)
       , Account(..)
       , Author(..)
       , AccountValidationException(..)
       , accountComputeFee
       , validateSaneArrival
       , validateSaneDeparture
       , checkThatAccountIsUpdatedOnly
       ) where

import Control.Monad.Error.Class (MonadError)
import Data.List as List (partition)
import Data.Map as Map (toList)
import qualified Data.Text.Buildable
import Formatting (bprint, build, int, (%))
import Snowdrop.Model.State.Core (ERoComp, HasKeyValue, PreValidator (..), StatePException,
                                  StateTx (..), StateTxType (..), TxValidationException (..),
                                  Validator, mkValidator, queryOne, validateIff)
import Snowdrop.Util


data AccountTx = AccountTxId deriving (Eq, Ord, Show, Generic)

-- | Slice of account that interest us while doing money transfers.
data Account = Account
    { aBalance :: Integer -- ^ Account balance.
    , aNonce   :: Integer -- ^ Count of transactions originated _from_ this account.
    } deriving (Eq, Ord, Show, Generic)

instance Buildable Account where
    build Account{..} = bprint ("account: bal "%int%", nonce "%int) aBalance aNonce

-- | Aggegate of author 'Account' information from tx.
data Author id = Author
    { auAuthorId :: id      -- ^ Raw author address.
    , auNonce    :: Integer -- ^ Number to match the nonce from author's 'Account'.
    } deriving (Eq, Ord, Show, Generic)

instance Buildable id => Buildable (Author id) where
    build Author{..} = bprint ("author "%build%", nonce "%int) auAuthorId auNonce

-- | Type for possible failures during transaction validation.
data AccountValidationException
    = AuthorDoesNotExist
    | SignatureIsMissing
    | SignatureIsCorrupted
    | KeysMismatch
    | TransactionIsCorrupted
    | NoncesMismatch
    | NotASingletonSelfUpdate      -- ^ 'Author' account updated multiple times.
    | NonceMustBeIncremented
    | PaymentMustBePositive
    | ReceiverDoesNotExist
    | ReceiverOnlyGetsMoney        -- ^ Receiver can only change its 'aBalance', never 'aNonce'.
    | ReceiverMustIncreaseBalance  -- ^ Receiver cannot decrease in its 'aBalance'.
    | SumMustBeNonNegative         -- ^ Amount of money sent must be greater of equal
                                   -- to the total amount received.
    deriving (Eq,Ord,Show)

-- | Wrapper for address.
newtype AccountId id = AccountId { unAccountId :: id }
    deriving (Eq, Ord, Show, Generic)

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

type ValidatorCtx e pk addr id proof value ctx sig hash stTxId
    =   ( HasExceptions e
           [ AccountValidationException
           , TxValidationException
           , StatePException
           ]
        , VerifySign pk sig (hash, pk)
        , HasGetter proof hash
        , HasGetter proof pk
        , HasGetter pk addr
        , HasGetter proof (WithSignature pk sig (hash, pk))
        , HasKeyValue id value (AccountId addr) Account
        , IdStorage stTxId AccountTx
        , IdSumPrefixed id
        , Ord addr
        , Ord id
        , Eq hash
        , Eq pk
        )

validateSimpleMoneyTransfer
    :: forall       e pk addr id proof value ctx sig hash stTxId
    .  ValidatorCtx e pk addr id proof value ctx sig hash stTxId
    => Validator    e         id proof value ctx

validateSimpleMoneyTransfer = mkValidator ty
    [preValidateSimpleMoneyTransfer @e @pk @addr @id @proof @value @ctx @sig @hash @stTxId]
  where
    ty = StateTxType $ getId (Proxy @stTxId) AccountTxId

authenticate
    :: forall       e pk addr id proof value ctx sig hash stTxId
    .  ValidatorCtx e pk addr id proof value ctx sig hash stTxId
    => Eq             pk
    => proof
    -> ERoComp      e         id       value ctx (AccountId addr, Account)
authenticate proof = do
    let signedHash :: WithSignature pk sig (hash, pk)
            = gett proof

        realHashfromExpander :: hash
            = gett proof

    (hash, pk) <- signedHash                   `assertSigned` SignatureIsCorrupted
    ()         <- pk == wsPublicKey signedHash `check`        KeysMismatch

    let authorId = gett pk

    before     <- AccountId authorId           `assertExists` AuthorDoesNotExist
    ()         <- realHashfromExpander == hash `check`        TransactionIsCorrupted

    return (AccountId authorId, before)

preValidateSimpleMoneyTransfer
    :: forall       e pk addr id proof value ctx sig hash stTxId
    .  ValidatorCtx e pk addr id proof value ctx sig hash stTxId
    => PreValidator e         id proof value ctx
preValidateSimpleMoneyTransfer =
    PreValidator $ \_trans@StateTx {..} -> do
        (author, before) <- authenticate @e @pk @addr @id @proof @value @ctx @sig @hash @stTxId txProof

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
accountComputeFee
  :: forall e addr id proof value ctx.
    ( HasExceptions e
        [ AccountValidationException
        , TxValidationException
        , StatePException
        ]
    , HasPrism proof (Author (AccountId addr))
    , HasKeyValue id value (AccountId addr) Account
    , IdSumPrefixed id
    , IdStorage id AccountTx
    , Ord addr
    , Ord id
    )
    => StateTx id proof value
    -> ERoComp e id value ctx Integer
accountComputeFee = \StateTx{..} -> do
    let ty = StateTxType $ getId (Proxy @id) AccountTxId
    author
        :: Author (AccountId addr)
        <- requirePart txProof (ProofProjectionFailed ty)
    before <- assertExists (auAuthorId author) AuthorDoesNotExist
    let updates = Map.toList $ changeSet txBody
    let accountChanges = updates <&> \pair@(k, _) -> (k, proj pair)
    changes <- mapM checkThatAccountIsUpdatedOnly accountChanges
    let (payer, recipients) = List.partition (is (auAuthorId author)) changes
    (paid, deltas) <-
        (,) <$> validateSaneDeparture payer before
            <*> mapM validateSaneArrival recipients
    let difference = paid - sum deltas
    () <- validateIff SumMustBeNonNegative (difference >= 0)
    return difference

validateSaneDeparture
  :: forall e id value addr ctx.
    ( HasException e AccountValidationException
    , Ord id
    )
    => [(AccountId addr, Account)]
    -> Account
    -> ERoComp e id value ctx Integer
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
  :: forall e id value addr ctx.
    ( HasExceptions e
        [ AccountValidationException
        , StatePException
        ]
    , HasKeyValue id value (AccountId addr) Account
    , Ord id
    , Ord addr
    )
    => (AccountId addr, Account)
    -> ERoComp e id value ctx Integer
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
  :: forall e id value addr ctx.
    ( HasReview e TxValidationException
    , IdSumPrefixed id
    )
    => (id, Maybe (AccountId addr, ValueOp Account))
    -> ERoComp e id value ctx (AccountId addr, Account)
checkThatAccountIsUpdatedOnly = \case
    (_, Just (accId, Upd it)) -> return (accId, it)
    (k, _)                    -> throwLocalError $ UnexpectedPayload [idSumPrefix k]

is :: Eq addr
    => AccountId addr
    -> (AccountId addr, Account)
    -> Bool
is accountId (accId, _) = accountId == accId
