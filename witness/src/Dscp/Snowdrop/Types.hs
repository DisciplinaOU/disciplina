{-# LANGUAGE StrictData #-}

-- | Snowdrop-related types.

module Dscp.Snowdrop.Types
    ( PublicationTxTypeId(..)
    , AccountTxTypeId(..)
    , BlockMetaTxTypeId(..)
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
    , BlockException(..)
    , _DuplicatedDifficulty
    , _DifficultyIsTooLarge
    , _PrevBlockIsIncorrect
    , _SlotIdIsNotIncreased
    ) where

import Control.Lens (makePrisms)
import Data.Text.Buildable (Buildable (..))
import Fmt (build, (+|), (|+))
import Snowdrop.Block (BlockApplicationException)
import Snowdrop.Util (HasReview (..))
import qualified Text.Show

import Dscp.Core

import Dscp.Snowdrop.Account

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

-- | Aggegate of author 'Account' information from tx.
data Author = Author
    { auAuthorId :: Address  -- ^ Raw author address.
    , auNonce    :: Integer  -- ^ Number to match the nonce from author's 'Account'.
    } deriving (Eq, Ord, Show, Generic)

instance Buildable Author where
    build Author{..} = "author " +| auAuthorId |+ ", nonce " +| auNonce |+ ""
