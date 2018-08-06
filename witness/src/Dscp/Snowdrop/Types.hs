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

import Data.Text.Buildable (Buildable (..))
import Formatting (bprint, build, int, (%))

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
    deriving (Eq, Ord, Show)

data AccountTxTypeId = AccountTxTypeId deriving (Eq, Ord, Show, Generic)

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
    deriving (Eq, Ord, Show)

-- | Wrapper for address.
newtype AccountId = AccountId { unAccountId :: Address }
    deriving (Eq, Ord, Show, Generic)

-- | Slice of account that interest us while doing money transfers.
data Account = Account
    { aBalance :: Integer  -- ^ Account balance.
    , aNonce   :: Integer  -- ^ Count of transactions originated _from_ this account.
    } deriving (Eq, Ord, Show, Generic)

instance Buildable Account where
    build Account{..} = bprint ("account: bal "%int%", nonce "%int) aBalance aNonce

-- | Aggegate of author 'Account' information from tx.
data Author = Author
    { auAuthorId :: Address  -- ^ Raw author address.
    , auNonce    :: Integer  -- ^ Number to match the nonce from author's 'Account'.
    } deriving (Eq, Ord, Show, Generic)

instance Buildable Author where
    build Author{..} = bprint ("author "%Formatting.build%", nonce "%int) auAuthorId auNonce
