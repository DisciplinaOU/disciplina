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
    , _PublicationFeeIsTooLow
    , _PublicationCantAffordFee
    , _PublicationLocalLoop
    , AccountException(..)
    , _MTxNoOutputs
    , _MTxDuplicateOutputs
    , _TransactionAlreadyExists
    , _InsufficientFees
    , _SignatureIsCorrupted
    , _NonceMismatch
    , _PaymentMustBePositive
    , _ReceiverOnlyGetsMoney
    , _OutputIsEmpty
    , _SumMustBeNonNegative
    , _CannotAffordFees
    , _CannotAffordOutputs
    , _InsufficientBalance
    , _WitnessMismatchesInput
    , BlockException(..)
    , _DuplicatedDifficulty
    , _DifficultyIsTooLarge
    , _PrevBlockIsIncorrect
    , _SlotIdIsNotIncreased
    ) where

import Control.Lens (LensLike', makePrisms, united)
import Data.Default (Default (..))
import Data.Text.Buildable (Buildable (..))
import Fmt (build, (+|), (|+))
import Snowdrop.Block (BlockApplicationException)
import Snowdrop.Util (HasReview (..))
import qualified Text.Show

import Dscp.Core

-- | Transaction type for publication.
data PublicationTxTypeId
    = PublicationTxTypeId
    deriving (Eq, Ord, Show, Generic)

data PublicationException
    = PublicationSignatureIsIncorrect
    | PublicationPrevBlockIsIncorrect
      { peGivenPrev :: Maybe PrivateHeaderHash, peActualTip :: Maybe PrivateHeaderHash }
    | PublicationFeeIsTooLow
      { peMinimalFee :: Integer, peGivenFee :: Integer }
    | PublicationCantAffordFee
      { peFee :: Integer, peBalance :: Integer }  -- ^ Publication owner can not afford the fee
    | PublicationLocalLoop
    | PublicationWitnessMismatchesAuthor
      { peSignerAddress :: Address, peAuthor :: Address }
    deriving (Eq, Ord)

makePrisms ''PublicationException

instance Show PublicationException where
    show = toString . pretty

instance Buildable PublicationException where
    build = \case
        PublicationSignatureIsIncorrect ->
            "Publication signature is incorrect"
        PublicationPrevBlockIsIncorrect{..} ->
            "Publication previous block is incorrect: given " +|
            (maybe "<genesis>" build peGivenPrev) +| ", tip was " +|
            (maybe "<genesis>" build peActualTip) +| ""
        PublicationFeeIsTooLow{..} ->
            "The fee specified in the publication tx " <> show peGivenFee <>
            " is lower than the minimal one " <> show peMinimalFee
        PublicationCantAffordFee{..} ->
            "Publication author can't afford the fee \
            \(fee: " +| peFee |+ ", balance: " +| peBalance |+ ")"
        PublicationLocalLoop ->
            "Transaction would create a loop in educator's chain"
        PublicationWitnessMismatchesAuthor{..} ->
            "Publication author " +| peAuthor |+ " does not correspond to public key in \
            \witness (address=" +| peSignerAddress |+ ")"

data AccountTxTypeId = AccountTxTypeId deriving (Eq, Ord, Show, Generic)

-- | Type for possible failures during transaction validation.
data AccountException
    = MTxNoOutputs
    | MTxDuplicateOutputs
    | TransactionAlreadyExists
      { taeTxId :: TxId }
    | InsufficientFees
      { aeExpectedFees :: Integer, aeActualFees :: Integer }
    | SignatureIsCorrupted
    | NonceMismatch
      { aePreviousNonce :: Nonce, aeTxNonce :: Nonce }
    | PaymentMustBePositive
    | ReceiverOnlyGetsMoney        -- ^ Receiver can only change its 'aBalance', never 'aNonce'.
    | OutputIsEmpty
      { aeAddress :: Address }
    | SumMustBeNonNegative
      { aeSent :: Integer, aeReceived :: Integer }
      -- ^ Amount of money sent must be greater of equal
      -- to the total amount received.
    | CannotAffordOutputs
      { aeOutputsSum :: Integer, aeBalance :: Integer }
    | CannotAffordFees
      { aeSpent :: Integer, aeBalance :: Integer }
      -- ^ Given account state cannot afford given fees.
    | WitnessMismatchesInput
      { aeSignerAddress :: Address, aeInput :: Address }
    deriving (Eq, Ord)

makePrisms ''AccountException

_InsufficientBalance
    :: (Applicative f, Semigroup (f AccountException))
    => LensLike' f AccountException ()
_InsufficientBalance = (_CannotAffordOutputs . united) <> (_CannotAffordFees . united)

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
        SignatureIsCorrupted ->
            "Bad signature"
        NonceMismatch{..} ->
            "Nonce does not match: previous nonce of input account was "
            +| aePreviousNonce |+ ", nonce used in transaction is " +| aeTxNonce |+ ""
        PaymentMustBePositive ->
            "Spent amount of money must be positive"
        ReceiverOnlyGetsMoney ->
            "Improper changes of receiver account (it is only possible to add \
            \tokens)"
        OutputIsEmpty{..} ->
            "Output corresponding to address " +| aeAddress |+ " is zero"
        SumMustBeNonNegative{..} ->
            "Tx input value (" +| unsafeMkCoin aeSent |+ ") is not greater than \
            \sum of outputs (" +| unsafeMkCoin aeReceived |+ ")"
        CannotAffordOutputs{..} ->
            "Tx sender can not afford outputs: outputs sum is " +| unsafeMkCoin aeOutputsSum
            |+ ", while balance is " +| unsafeMkCoin aeBalance |+ ""
        CannotAffordFees{..} ->
            "Tx sender can not afford fees: sending " +| unsafeMkCoin aeSpent |+
            ", while balance is " +| unsafeMkCoin aeBalance |+ ""
        WitnessMismatchesInput{..} ->
            "Transaction input " +| aeInput |+ " does not correspond to public key in \
            \witness (address=" +| aeSignerAddress |+ ")"

instance Show AccountException where
    show = toString . pretty

-- | Transaction type for block metas.
data BlockMetaTxTypeId = BlockMetaTxTypeId deriving (Eq, Ord, Show, Generic)

data BlockException
    = DuplicatedDifficulty
      { bmeProvidedHeader :: Header, bmeExistingHeaderHash :: HeaderHash }
    | DifficultyIsTooLarge
      { bmeDifficulty :: Difficulty }
    | PrevBlockIsIncorrect
      { bmeProvidedHash :: HeaderHash, bmeTipHash :: HeaderHash }
    | SlotIdIsNotIncreased
      { bmeProvidedSlotId :: SlotId, bmeTipSlotId :: SlotId }
    | InvalidBlockSignature
    | IssuerDoesNotOwnSlot
      { bmrSlotId :: SlotId, bmrIssuer :: Address }
    | BlockApplicationError (BlockApplicationException HeaderHash)
    | BlockMetaInternalError Text

makePrisms ''BlockException

instance Buildable BlockException where
    build = \case
        DuplicatedDifficulty{..} ->
            "Block with this difficulty already exists: provided " +| bmeProvidedHeader |+
            ", but another block " +| bmeExistingHeaderHash |+ " already exists in chain."
        DifficultyIsTooLarge{..} ->
            "Difficulty should've been incremented by one, but is larger: "
            +| bmeDifficulty |+ ""
        PrevBlockIsIncorrect{..} ->
            "Previous block is incorrect: expected " +| bmeTipHash |+
            ", given " +| bmeProvidedHash |+ ""
        SlotIdIsNotIncreased{..} ->
            "Slot id should've been increased: provided " +| bmeProvidedSlotId |+
            ", current tip was created at " +| bmeTipSlotId |+ ""
        InvalidBlockSignature ->
            "Block signature is invalid"
        IssuerDoesNotOwnSlot{..} ->
            "Node " +| bmrIssuer |+ " does not own slot " +| bmrSlotId |+ ""
        BlockApplicationError err ->
            build err
        BlockMetaInternalError msg ->
            "Internal error: " +| msg |+ ""

instance HasReview BlockException (BlockApplicationException HeaderHash) where
    inj = BlockApplicationError


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
