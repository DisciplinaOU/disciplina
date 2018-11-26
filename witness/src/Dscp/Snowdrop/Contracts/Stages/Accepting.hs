
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

makeLenses ''ContractAccepting

checkBuyerAccepts
    ::  ( HasPrism Proofs (PersonalisedProof ContractAcceptingTxId ContractAccepting)
        , HasPrism Proofs ContractAcceptingTxId
        , HasGetter Crypto.PublicKey Address
        )
    =>  PreValidator Exceptions Ids Proofs Values ctx
checkBuyerAccepts =
    PreValidator $ \StateTx { txProof } -> do
        Authenticated buyer _ ContractAccepting
            { _ciaContractID = cid
            } _txFees
            <- authenticate @ContractAcceptingTxId txProof

        _ <- accessContractAsBuyerAtStage buyer cid Transmitted

        return ()
