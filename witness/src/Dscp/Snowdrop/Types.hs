-- | Snowdrop-related types.

module Dscp.Snowdrop.Types
    ( PublicationTxTypeId(..)
    , AccountTxTypeId(..)
    , AccountId(..)
    , Account(..)
    , Author(..)
    , PublicationException(..)
    , _PublicationSignatureIsIncorrect
    , _PublicationPrevBlockIsIncorrect
    , _StorageIsCorrupted
    , _PublicationIsBroken
    , _PublicationFeeIsTooLow
    , _PublicationCantAffordFee
    , _PublicationLocalLoop
    , AccountException(..)
    , _MTxNoOutputs
    , _MTxDuplicateOutputs
    , _TransactionAlreadyExists
    , _InsufficientFees
    , _SignatureIsMissing
    , _SignatureIsCorrupted
    , _TransactionIsCorrupted
    , _NotASingletonSelfUpdate
    , _NonceMustBeIncremented
    , _PaymentMustBePositive
    , _ReceiverOnlyGetsMoney
    , _ReceiverMustIncreaseBalance
    , _SumMustBeNonNegative
    , _BalanceCannotBecomeNegative
    , _CannotAffordFees
    ) where

import Control.Lens (makePrisms)
import Data.Default (Default (..))
import Data.Text.Buildable (Buildable (..))
import Fmt (build, (+|), (|+))
import qualified Text.Show

import Dscp.Core

-- | Transaction type for publication.
data PublicationTxTypeId
    = PublicationTxTypeId
    deriving (Eq, Ord, Show, Generic)

data PublicationException
    = PublicationSignatureIsIncorrect
    | PublicationPrevBlockIsIncorrect
    | StorageIsCorrupted
    | PublicationIsBroken Text
    | PublicationFeeIsTooLow
      { peMinimalFee :: Integer, peGivenFee :: Integer }
    | PublicationCantAffordFee
      { peFee :: Integer, peBalance :: Integer }  -- ^ Publication owner can not afford the fee
    | PublicationLocalLoop
    deriving (Eq, Ord)

makePrisms ''PublicationException

instance Show PublicationException where
    show = toString . pretty

instance Buildable PublicationException where
    build = \case
        PublicationSignatureIsIncorrect ->
            "Publication signature is incorrect"
        PublicationPrevBlockIsIncorrect ->
            "Publication previous block is incorrect"
        StorageIsCorrupted ->
            "Storage is inconsistent"
        PublicationIsBroken msg ->
            "Bad publication" +| msg |+ ""
        PublicationFeeIsTooLow{..} ->
            "The fee specified in the publication tx " <> show peGivenFee <>
            " is lower than the minimal one " <> show peMinimalFee
        PublicationCantAffordFee{..} ->
            "Publication author can't afford the fee \
            \(fee: " +| peFee |+ ", balance: " +| peBalance |+ ")"
        PublicationLocalLoop ->
            "Transaction would create a loop in educator's chain"

data AccountTxTypeId = AccountTxTypeId deriving (Eq, Ord, Show, Generic)

-- | Type for possible failures during transaction validation.
data AccountException
    = MTxNoOutputs
    | MTxDuplicateOutputs
    | TransactionAlreadyExists
      { taeTxId :: TxId }
    | InsufficientFees
      { aeExpectedFees :: Integer, aeActualFees :: Integer }
    | SignatureIsMissing
    | SignatureIsCorrupted
    | TransactionIsCorrupted
    | NotASingletonSelfUpdate      -- ^ 'Author' account updated multiple times.
    | NonceMustBeIncremented
      { aePreviousNonce :: Nonce, aeNewNonce :: Nonce }
    | PaymentMustBePositive
    | ReceiverOnlyGetsMoney        -- ^ Receiver can only change its 'aBalance', never 'aNonce'.
    | ReceiverMustIncreaseBalance  -- ^ Receiver cannot decrease in its 'aBalance'.
    | SumMustBeNonNegative
      { aeSent :: Integer, aeReceived :: Integer, aeFees :: Integer }
      -- ^ Amount of money sent must be greater of equal
      -- to the total amount received.
    | CannotAffordFees
      { aeOutputsSum :: Integer, aeBalance :: Integer, aeFees :: Integer }
      -- ^ Given account state cannot afford given fees.
    | BalanceCannotBecomeNegative
      { aeSpent :: Integer, aeBalance :: Integer }
    | AccountInternalError String
    deriving (Eq, Ord)

makePrisms ''AccountException

instance Buildable AccountException where
    build = \case
        MTxNoOutputs ->
            "Transaction has no outputs"
        MTxDuplicateOutputs ->
            "Duplicated transaction outputs"
        TransactionAlreadyExists{..} ->
            "Transaction " +| taeTxId |+ " has already been registered"
        InsufficientFees{..} ->
            "Amount of money left for fees in transaction is not enough, \
             \expected " +| unsafeMkCoin aeExpectedFees |+ ", got " +| unsafeMkCoin aeActualFees |+ ""
        SignatureIsMissing ->
            "Transaction has no correct signature"
        SignatureIsCorrupted ->
            "Bad signature"
        TransactionIsCorrupted ->
            "Transaction is corrupted"
        NotASingletonSelfUpdate ->
            "Author account is updated multiple times"
        NonceMustBeIncremented{..} ->
            "Nonce should've been incremented by one: previous nonce was "
            +| aePreviousNonce |+ ", new nonce is " +| aeNewNonce |+ ""
        PaymentMustBePositive ->
            "Spent amount of money must be positive"
        ReceiverOnlyGetsMoney ->
            "Improper changes of receiver account (it is only possible to add \
            \tokens)"
        ReceiverMustIncreaseBalance ->
            "One of receivers' balance decreased or didn't change"
        SumMustBeNonNegative{..} ->
            "Tx input value (" +| unsafeMkCoin aeSent |+ ") is not greater than \
            \sum of outputs (" +| unsafeMkCoin aeReceived |+ ") plus fees (" +| unsafeMkCoin aeFees |+ ")"
        CannotAffordFees{..} ->
            "Tx sender can not afford fees: sending " +| unsafeMkCoin aeOutputsSum |+ " \
            \and fees are " +| unsafeMkCoin aeFees |+ ", while balance is " +| unsafeMkCoin aeBalance |+ ""
        BalanceCannotBecomeNegative{..} ->
            "Balance can not become negative: spending " +| unsafeMkCoin aeSpent |+ ", \
            \while balance is " +| unsafeMkCoin aeBalance |+ ""
        AccountInternalError s ->
            fromString $ "Expander failed internally: " <> s

instance Show AccountException where
    show = toString . pretty

-- | Wrapper for address.
newtype AccountId = AccountId { unAccountId :: Address }
    deriving (Eq, Ord, Show, Generic)

-- | Slice of account that interest us while doing money transfers.
data Account = Account
    { aBalance :: Integer  -- ^ Account balance.
    , aNonce   :: Nonce    -- ^ Account nonce.
    } deriving (Eq, Ord, Show, Generic)

-- | How absense of account in db should look like outside.
instance Default Account where
    def = Account{ aBalance = 0, aNonce = 0 }

instance Buildable Account where
    build Account{..} = "account: bal " +| aBalance |+ ", nonce " +| aNonce |+ ""

-- | Aggegate of author 'Account' information from tx.
data Author = Author
    { auAuthorId :: Address  -- ^ Raw author address.
    , auNonce    :: Integer  -- ^ Number to match the nonce from author's 'Account'.
    } deriving (Eq, Ord, Show, Generic)

instance Buildable Author where
    build Author{..} = "author " +| auAuthorId |+ ", nonce " +| auNonce |+ ""
