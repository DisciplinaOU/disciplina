
module Dscp.Snowdrop.Contracts.Stages.Creation where

import Dscp.Snowdrop.Contracts.Account
import Dscp.Snowdrop.Contracts.Util

data ContractCreationRequest = ContractCreationRequest
    { _acContractID       :: ContractID
    , _acContractBody     :: ContractAccount
    , _acMoneyTransmitted :: Coin
    }

makeLenses ''ContractCreationRequest

checkCreation :: () => PreValidate ctx
checkCreation =
    PreValidator $ \StateTx { txProof } -> do
        Authenticated buyer _ ContractCreationRequest
            { _acContractID       = cid
            , _acContractBody     = body
            , _acMoneyTransmitted = money
            } fees
            <- authenticate @_ txProof

        seller <- getPublicationAuthor (body^.caPublication)

        ()     <- assertAbsence cid AddressIsAlreadyTaken

        ()     <- check (seller                      /= buyer)   SelfContractsUnallowed
        ()     <- check (body^.caSeller              == seller)  WrongSeller
        ()     <- check (body^.caBuyer               == buyer)   WrongBuyer
        ()     <- check (body^.caCost + body^.caFees <= money)   ThisCostsMore
        ()     <- check (body^.caStage               == Created) WrongIntialStage
        ()     <- check (body^.caInheritor           == buyer)   WrongInheritor

        ()     <- assertCorrectCostAndFees body
        ()     <- assertSlotIsInFuture (body^.caEndSlot)

        return ()

