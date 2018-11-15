
module Dscp.Snowdrop.Contracts.Account where

import Dscp.Snowdrop.Contracts.Util

data Stage
    = Created
    | Cancelled
    | Transmitted
    | KeySent
    | Arbitration
    | Agreed

-- TODO (kirill.andreev): Find a place for these.
data ContractAccount = ContractAccount
    { _caSelf          :: Account
    , _caSeller        :: AccountId
    , _caBuyer         :: AccountId
    , _caCost          :: Coin
    , _caFees          :: Coin
    , _caPublication   :: PublicationHead
    , _caEndSlot       :: SlotId
    , _caInheritor     :: AccountId
    , _caStage         :: Stage
    }

newtype ContractID = ContractID { getContractID :: Address }

type instance SValue ContractID = ContractAccount

makeLenses ''ContractAccount
