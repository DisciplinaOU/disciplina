
module Dscp.Snowdrop.Contracts.Stages.Accepting where

import Dscp.Crypto as Crypto
import Dscp.Snowdrop.Contracts.Contract
import Dscp.Snowdrop.Contracts.Entities
import Dscp.Snowdrop.Contracts.Util

data ContractAcceptingTxId = ContractAcceptingTxId
    deriving (Eq, Ord, Show, Generic)

data ContractAccepting = ContractAccepting
    { _ciaContractID :: ContractID
    }
    deriving (Eq, Show, Generic)

instance Serialise ContractAcceptingTxId
instance Serialise ContractAccepting

type ContractAcceptingWitness = PersonalisedProof ContractAcceptingTxId ContractAccepting

makeLenses ''ContractAccepting

checkAccepting
    ::  ( HasPrism Proofs ContractAcceptingWitness
        , HasPrism Proofs ContractAcceptingTxId
        , HasGetter Crypto.PublicKey Address
        )
    =>  PreValidator Exceptions Ids Proofs Values ctx
checkAccepting =
    PreValidator $ \StateTx { txProof } -> do
        Authenticated buyer _ ContractAccepting
            { _ciaContractID = cid
            } _txFees
            <- authenticate @ContractAcceptingTxId txProof

        _ <- accessContractAsBuyerAtStage buyer cid Transmitted

        return ()

expandAccepting
    ::  ( HasPrism Proofs ContractAcceptingWitness
        , HasPrism Proofs ContractAcceptingTxId
        , HasGetter Crypto.PublicKey Address
        )
    => AccountId
    -> Expand ctx ContractAcceptingWitness
expandAccepting _miner =
    SeqExpanders $ one $ Expander
        (error "TODO: add read prefices")
        (error "TODO: add write prefices")
        $ \txProof -> do
            Authenticated buyer _ ContractAccepting
                { _ciaContractID = cid
                } _fees
                    <- authenticate @ContractAcceptingTxId (inj txProof)

            contract <- accessContractAsBuyerAtStage buyer cid Transmitted

            merge
                [ setStage cid Accepted
                , pay
                    (contract^.caSelf)
                    (coinToInteger $
                        (contract^.caCost) `unsafeAddCoin` (contract^.caFees))
                    (contract^.caSeller)

                , pay
                    (contract^.caSelf)
                    (coinToInteger $ contract^.caFees)
                    (contract^.caBuyer)
                ]
