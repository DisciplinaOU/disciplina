
module Dscp.Snowdrop.Contracts.Stages.Creation where

import Dscp.Crypto as Crypto
import Dscp.Snowdrop.Contracts.Contract
import Dscp.Snowdrop.Contracts.Entities
import Dscp.Snowdrop.Contracts.Util

data ContractCreationTxId = ContractCreationTxId
    deriving (Eq, Ord, Show, Generic)

data ContractCreation = ContractCreation
    { _acContractID       :: ContractID
    , _acContractBody     :: Contract
    , _acMoneyTransmitted :: Coin
    }
    deriving (Eq, Show, Generic)

instance Serialise ContractCreationTxId
instance Serialise ContractCreation

makeLenses ''ContractCreation

checkCreation
    ::  ( HasPrism Proofs (PersonalisedProof ContractCreationTxId ContractCreation)
        , HasPrism Proofs  ContractCreationTxId
        , HasGetter Crypto.PublicKey Address
        )
    => PreValidator Exceptions Ids Proofs Values ctx
checkCreation =
    PreValidator $ \StateTx { txProof } -> do
        Authenticated buyer _ ContractCreation
            { _acContractID       = cid
            , _acContractBody     = body
            , _acMoneyTransmitted = money
            } _fees
            <- authenticate @ContractCreationTxId txProof

        seller <- getPublicationAuthor (body^.caPublication)

        ()     <- assertAbsence cid (AddressIsAlreadyTaken cid)

        ()     <- check (seller                      /= buyer)   (SelfContractsUnallowed cid buyer)
        ()     <- check (body^.caSeller              == seller)  (WrongSeller cid seller)
        ()     <- check (body^.caBuyer               == buyer)   (WrongBuyer  cid buyer)
        ()     <- check (body^.caStage               == Created) (WrongInitialStage cid)
        ()     <- check (body^.caInheritor           == buyer)   (WrongInheritor cid (body^.caInheritor))

        requiredSum <- case (body^.caCost) `addCoins` (body^.caFees) of
            Left reason ->
                throwLocalError (CannotSumCostAndFees cid reason)

            Right reqSum ->
                return reqSum

        ()     <- check (requiredSum <= money) (ThisCostsMore cid requiredSum money)

        ()     <- assertCorrectCostAndFees body
        ()     <- assertSlotIsInFuture (body^.caEndSlot)

        return ()

