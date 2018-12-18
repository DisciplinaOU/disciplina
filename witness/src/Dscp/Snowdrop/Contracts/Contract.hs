
module Dscp.Snowdrop.Contracts.Contract where

import Codec.Serialise
import Control.Lens (makeLenses)

import Fmt (build)

import Dscp.Core
import Dscp.Educator.DB.DSL.Class (QueryTxs)
import Dscp.Snowdrop.Types
import Dscp.Snowdrop.Storage.Types

data Stage
    = Created
    | Started
    | Cancelled
    | Transmitted
    | KeySent
    | BuyerCheated
    | SellerCheated
    | SellerDuped
    | Accepted
    deriving (Eq, Show, Generic)

-- TODO (kirill.andreev): Find a place for these.
data Contract = Contract
    { _caSelf          :: AccountId
    , _caSeller        :: AccountId
    , _caBuyer         :: AccountId
    , _caCost          :: Coin
    , _caFees          :: Coin
    , _caMoney         :: Integer
    , _caEndSlot       :: SlotId
    , _caStage         :: Stage
    , _caEncryptedKey  :: EncryptedKey
    , _caQuery         :: QueryTxs
    }
    deriving (Eq, Show, Generic)

type EncryptedKey = ByteString

newtype ContractID = ContractID { getContractID :: Address }
    deriving (Eq, Ord, Show, Generic)

instance Buildable ContractID where
    build = show

instance Serialise AccountId => Serialise Contract
instance Serialise ContractID
instance Serialise Stage

makeLenses ''Contract
