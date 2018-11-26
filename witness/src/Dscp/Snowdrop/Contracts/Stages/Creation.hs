
module Dscp.Snowdrop.Contracts.Stages.Creation where

import Dscp.Crypto as Crypto
import Dscp.Snowdrop.Contracts.Contract
import Dscp.Snowdrop.Contracts.Entities
import Dscp.Snowdrop.Contracts.Util

data ContractCreationTxId = ContractCreationTxId
    deriving (Eq, Ord, Show, Generic)

data ContractCreation = ContractCreation
    { _ccContractID       :: ContractID
    , _ccContractBody     :: Contract
    , _ccMoneyTransmitted :: Coin
    }
    deriving (Eq, Show, Generic)

instance Serialise ContractCreationTxId
instance Serialise ContractCreation

type ContractCreationWitness = PersonalisedProof ContractCreationTxId ContractCreation

makeLenses ''ContractCreation

checkCreation
    ::  ( HasPrism Proofs ContractCreationWitness
        , HasPrism Proofs ContractCreationTxId
        , HasGetter Crypto.PublicKey Address
        )
    => PreValidator Exceptions Ids Proofs Values ctx
checkCreation =
    PreValidator $ \StateTx { txProof } -> do
        Authenticated buyer _ ContractCreation
            { _ccContractID       = cid
            , _ccContractBody     = body
            , _ccMoneyTransmitted = money
            } _fees
            <- authenticate @ContractCreationTxId txProof

        seller <- getPublicationAuthor (body^.caPublication)

        ()     <- assertAbsence cid (AddressIsAlreadyTaken cid)

        ()     <- check (seller            /= buyer)   (SelfContractsUnallowed cid buyer)
        ()     <- check (body^.caSeller    == seller)  (WrongSeller    cid  seller)
        ()     <- check (body^.caBuyer     == buyer)   (WrongBuyer     cid  buyer)
        ()     <- check (body^.caStage     == Created) (WrongStage     cid (body^.caStage))
        ()     <- check (body^.caInheritor == buyer)   (WrongInheritor cid (body^.caInheritor))

        requiredSum <- case (body^.caCost) `addCoins` (body^.caFees) of
            Left reason ->
                throwLocalError (CannotSumCostAndFees cid reason)

            Right reqSum ->
                return reqSum

        ()     <- check (requiredSum <= money) (ThisCostsMore cid requiredSum money)

        ()     <- assertCorrectCostAndFees body
        ()     <- assertSlotIsInFuture (body^.caEndSlot)

        return ()

expandCreation
    ::  ( HasPrism Proofs ContractCreationWitness
        , HasPrism Proofs ContractCreationTxId
        , HasGetter Crypto.PublicKey Address
        )
    => AccountId
    -> Expand ctx ContractCreationWitness
expandCreation _miner =
    SeqExpanders $ one $ Expander
        (error "TODO: add read prefices")
        (error "TODO: add write prefices")
        $ \txProof -> do
            Authenticated buyer _ ContractCreation
                { _ccContractID       = cid
                , _ccContractBody     = body
                , _ccMoneyTransmitted = money
                } _fees
                    <- authenticate @ContractCreationTxId (inj txProof)

            merge
                [ pay buyer (coinToInteger money) (body^.caSelf)
                , createContract cid body
                , createAccount (body^.caSelf) def
                -- , pay seller (coinToInteger _txFees) miner
                ]

