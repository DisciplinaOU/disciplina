-- | Snowdrop-related types.

module Dscp.Snowdrop.Types
    ( PublicationTxTypeId(..)
    , PublicationValidationException(..)
    , AccountTxTypeId(..)
    , AccountValidationException(..)
    , AccountId(..)
    , Account(..)
    , Author(..)
    ) where

import Data.Default (Default (..))
import Data.Text.Buildable (Buildable (..))
import Formatting (bprint, build, int, (%))
import qualified Text.Show

import Dscp.Core.Foundation (Address)

-- | Transaction type for publication.
data PublicationTxTypeId
    = PublicationTxTypeId
    deriving (Eq, Ord, Show, Generic)

data PublicationValidationException
    = PublicationSignatureIsIncorrect
    | PublicationPrevBlockIsIncorrect
    | StorageIsCorrupted
    | PublicationIsBroken
    deriving (Eq, Ord)

instance Show PublicationValidationException where
    show = toString . pretty

instance Buildable PublicationValidationException where
    build = \case
        PublicationSignatureIsIncorrect -> "Publication signature is incorrect"
        PublicationPrevBlockIsIncorrect -> "Publication previous block is incorrect"
        StorageIsCorrupted -> "Storage is inconsistent"
        PublicationIsBroken -> "Bad publication"

data AccountTxTypeId = AccountTxTypeId deriving (Eq, Ord, Show, Generic)

-- | Type for possible failures during transaction validation.
--
-- NOTE: this exception is thrown by witness API, keep 'witness.yaml' doc
-- updated.
data AccountValidationException
    = AuthorDoesNotExist
    | SignatureIsMissing
    | SignatureIsCorrupted
    | TransactionIsCorrupted
    | NotASingletonSelfUpdate      -- ^ 'Author' account updated multiple times.
    | NonceMustBeIncremented
    | PaymentMustBePositive
    | ReceiverOnlyGetsMoney        -- ^ Receiver can only change its 'aBalance', never 'aNonce'.
    | ReceiverMustIncreaseBalance  -- ^ Receiver cannot decrease in its 'aBalance'.
    | SumMustBeNonNegative         -- ^ Amount of money sent must be greater of equal
                                   -- to the total amount received.
    deriving (Eq, Ord, Enum, Bounded, Show)

instance Buildable AccountValidationException where
    build = \case
        AuthorDoesNotExist -> "Source account has never received any money"
        SignatureIsMissing -> "Transaction has no correct signature"
        SignatureIsCorrupted -> "Bad signature"
        TransactionIsCorrupted -> "Transaction is corrupted"
        NotASingletonSelfUpdate -> "Author account is updated multiple times"
        NonceMustBeIncremented -> "Nonce should've been incremented by one"
        PaymentMustBePositive -> "Spent amount of money must be positive"
        ReceiverOnlyGetsMoney -> "Improper changes of receiver account (its is \
                                 \only possible to add tokens)"
        ReceiverMustIncreaseBalance -> "Receiver's balance decreased"
        SumMustBeNonNegative -> "Tx input value < tx sum of outputs"

-- | Wrapper for address.
newtype AccountId = AccountId { unAccountId :: Address }
    deriving (Eq, Ord, Show, Generic)

-- | Slice of account that interest us while doing money transfers.
data Account = Account
    { aBalance :: Integer  -- ^ Account balance.
    , aNonce   :: Integer  -- ^ Count of transactions originated _from_ this account.
    } deriving (Eq, Ord, Show, Generic)

-- | How absense of account in db should look like outside.
instance Default Account where
    def = Account{ aBalance = 0, aNonce = 0 }

instance Buildable Account where
    build Account{..} = bprint ("account: bal "%int%", nonce "%int) aBalance aNonce

-- | Aggegate of author 'Account' information from tx.
data Author = Author
    { auAuthorId :: Address  -- ^ Raw author address.
    , auNonce    :: Integer  -- ^ Number to match the nonce from author's 'Account'.
    } deriving (Eq, Ord, Show, Generic)

instance Buildable Author where
    build Author{..} = bprint ("author "%Formatting.build%", nonce "%int) auAuthorId auNonce
