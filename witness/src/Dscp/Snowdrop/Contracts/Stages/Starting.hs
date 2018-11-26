
module Dscp.Snowdrop.Contracts.Stages.Starting where

import Dscp.Crypto as Crypto
import Dscp.Snowdrop.Contracts.Contract
import Dscp.Snowdrop.Contracts.Util
import Dscp.Snowdrop.Contracts.Entities

data ContractStartingTxId = ContractStartingTxId
    deriving (Eq, Ord, Show, Generic)

data ContractStarting = ContractStarting
    { _csrContractID :: ContractID
    , _csrFeesPaid   :: Coin
    }
    deriving (Eq, Ord, Show, Generic)

type ContractStartingWitness = PersonalisedProof ContractStartingTxId ContractStarting

instance Serialise ContractStartingTxId
instance Serialise ContractStarting

makeLenses ''ContractStarting

checkStartingTheTrade
    ::  ( HasPrism Proofs ContractStartingWitness
        , HasPrism Proofs ContractStartingTxId
        , HasGetter Crypto.PublicKey Address
        )
    => PreValidator Exceptions Ids Proofs Values ctx
checkStartingTheTrade =
    PreValidator $ \StateTx { txProof } -> do
        Authenticated seller _ ContractStarting
            { _csrContractID = cid
            , _csrFeesPaid   = fees
            } _txFees
            <- authenticate @ContractStartingTxId txProof

        contract <- accessContractAsSellerAtStage seller cid Created

        () <- check (seller == contract^.caSeller) (WrongSeller cid seller)
        () <- check (fees   <= contract^.caFees)   (SellerUnderpaidTheFees cid fees)

        -- I assume here that expander sets correct stage here

        return ()

expandStartingTheTrade
    ::  ( HasPrism Proofs ContractStartingWitness
        , HasPrism Proofs ContractStartingTxId
        , HasGetter Crypto.PublicKey Address
        )
    => AccountId
    -> Expand ctx ContractStartingWitness
expandStartingTheTrade _miner =
    SeqExpanders $ one $ Expander
        (error "TODO: add read prefices")
        (error "TODO: add write prefices")
        $ \txProof -> do
            Authenticated seller _ ContractStarting
                { _csrContractID = cid
                , _csrFeesPaid   = fees
                } _txFees
                    <- authenticate @ContractStartingTxId (inj txProof)

            contract <- accessContractAsSellerAtStage seller cid Created

            merge
                [ pay seller (coinToInteger fees)   (contract^.caSelf)
                -- , pay seller (coinToInteger _txFees) miner
                ]

