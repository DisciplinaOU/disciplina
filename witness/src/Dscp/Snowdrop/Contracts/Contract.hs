
module Dscp.Snowdrop.Contracts.Contract where

import Codec.Serialise
import Control.Lens (makeLenses)

import Fmt (build)

import Dscp.Core
import Dscp.Snowdrop.Types
import Dscp.Snowdrop.Storage.Types

data Stage
    = Created
    | Cancelled
    | Transmitted
    | KeySent
    | Arbitration
    | Agreed
    deriving (Eq, Show, Generic)

-- TODO (kirill.andreev): Find a place for these.
data Contract = Contract
    { _caSelf          :: AccountId
    , _caSeller        :: AccountId
    , _caBuyer         :: AccountId
    , _caCost          :: Coin
    , _caFees          :: Coin
    , _caPublication   :: PublicationHead
    , _caEndSlot       :: SlotId
    , _caInheritor     :: AccountId
    , _caStage         :: Stage
    }
    deriving (Eq, Show, Generic)

newtype ContractID = ContractID { getContractID :: Address }
    deriving (Eq, Ord, Show, Generic)

instance Buildable ContractID where
    build = show

instance Serialise AccountId => Serialise Contract
instance Serialise ContractID
instance Serialise Stage

makeLenses ''Contract
