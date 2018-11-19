
module Dscp.Snowdrop.Contracts.Stages.Starting where

import Dscp.Crypto as Crypto
import Dscp.Snowdrop.Contracts.Contract
import Dscp.Snowdrop.Contracts.Util
import Dscp.Snowdrop.Contracts.Entities

data ContractStartingTxId = ContractStartingTxId
    deriving (Eq, Ord, Show, Generic)

data ContractStartingRequest = ContractStartingRequest
    { _csrContractID :: ContractID
    , _csrFeesPaid   :: Coin
    }
    deriving (Eq, Ord, Show, Generic)

instance Serialise ContractStartingTxId
instance Serialise ContractStartingRequest

makeLenses ''ContractStartingRequest

checkStartingTheTrade
    ::  ( HasPrism Proofs (PersonalisedProof ContractStartingTxId ContractStartingRequest)
        , HasPrism Proofs  ContractStartingTxId
        , HasGetter Crypto.PublicKey Address
        )
    => PreValidator Exceptions Ids Proofs Values ctx
checkStartingTheTrade =
    PreValidator $ \StateTx { txProof } -> do
        Authenticated seller _ ContractStartingRequest
            { _csrContractID = cid
            , _csrFeesPaid   = fees
            } _txFees
            <- authenticate @ContractStartingTxId txProof

        contract <- getContract cid

        () <- check (seller == contract^.caSeller) (WrongSeller cid seller)
        () <- check (fees   <= contract^.caFees)   (SellerUnderpaidTheFees cid fees)

        -- I assume here that expander sets correct stage here

        return ()

