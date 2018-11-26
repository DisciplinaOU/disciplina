
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
    ::  ( HasPrism Proofs (PersonalisedProof ContractStartingTxId ContractStarting)
        , HasPrism Proofs  ContractStartingTxId
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
    ::  ( HasPrism Proofs (PersonalisedProof ContractStartingTxId ContractStarting)
        , HasPrism Proofs  ContractStartingTxId
        , HasGetter Crypto.PublicKey Address
        )
    => AccountId
    -> Expand ctx ContractStartingWitness
expandStartingTheTrade miner =
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
                , pay seller (coinToInteger _txFees) miner
                ]
            -- let selfAccount = contract^.caSelf

            -- self <- retrieve selfAccount (NoLinkedAccount cid selfAccount)

            -- let delta = DiffChangeSet $ fromList
            --         [ cid         ==> contract { stage    = Started }
            --         , selfAccount ==> self     { aBalance = aBalance self + fees }
            --         ]

pay :: AccountId -> Integer -> AccountId -> SDActionM ctx Delta
pay who howMuch whom = merge
    [ changeBalance who  (- howMuch)
    , changeBalance whom (  howMuch)
    ]

changeBalance :: AccountId -> Integer -> SDActionM ctx Delta
changeBalance aid change = do
    old <- retrieve aid (AccountDoesNotExist (show aid))
    let new = old { aBalance = aBalance old + change }

    () <- check (aBalance new >= 0) $
        BalanceCannotBecomeNegative change (aBalance old)

    return $ delta [ aid ==> Upd new ]
