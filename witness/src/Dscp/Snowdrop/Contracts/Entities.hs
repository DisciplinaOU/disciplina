
module Dscp.Snowdrop.Contracts.Entities where

import Dscp.Snowdrop.Contracts.Account
import Dscp.Snowdrop.Contracts.Utils

getContract :: ContractID -> SDActionM ctx ContractAccount
getContract cid =
    queryOne @Ids @ContractID @Values @ContractAccount cid
        >>= maybe (throwLocalError ContractDoesNotExist) pure

