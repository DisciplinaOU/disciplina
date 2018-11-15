
module Dscp.Snowdrop.Contracts.Stages.Accepting where

import Dscp.Snowdrop.Contracts.Account

data ContractIsAccepted = ContractIsAccepted
    { _ciaContractID :: ContractID
    }

makeLenses ''ContractIsAccepted

checkBuyerAccepts =
    PreValidator $ \StateTx { txProof } -> do
        Authenticated buyer _ ContractIsAccepted
            { _ciaContractID = cid
            } txFees
            <- authenticate @_ txProof

        contract <- getContract cid

        () <- check (buyer == contract^.caBuyer) WrongBuyer

        return ()
