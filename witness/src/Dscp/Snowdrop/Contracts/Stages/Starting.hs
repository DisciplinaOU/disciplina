
module Dscp.Snowdrop.Contracts.Stages.Starting where

import Dscp.Snowdrop.Contracts.Account
import Dscp.Snowdrop.Contracts.Util
import Dscp.Snowdrop.Contracts.Entities

data ContractStartingRequest = ContractStartingRequest
    { _csrContractID :: ContractID
    , _csrFeesPaid   :: Coin
    }

makeLenses ''ContractStartingRequest

checkStartingTheTrade =
    PreValidator $ \StateTx { txProof } -> do
        Authenticated seller _ ContractStartingRequest
            { _csrContractID = cid
            , _csrFeesPaid   = fees
            } txFees
            <- authenticate @_ txProof

        contract <- getContract cid

        () <- check (seller == contract^.caSeller) WrongPersonToStartTheTrade
        () <- check (fees   <= contract^.caFees)   SellerUnderpaidTheFees

        -- I assume here that expander sets correct stage here

        return ()

