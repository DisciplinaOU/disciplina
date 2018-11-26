
module Dscp.Snowdrop.Contracts.Stages.BrokenChunk where

import Dscp.Crypto as Crypto
import Dscp.Snowdrop.Contracts.Contract
import Dscp.Snowdrop.Contracts.Entities
import Dscp.Snowdrop.Contracts.Util

data TxId = ContractBrokenChunkTxId
    deriving (Eq, Ord, Show, Generic)

data Tx = ContractBrokenChunk
    { _cbcContractID :: ContractID
    , _cbcEncrypted  :: ByteString
    , _cbcSecretKey  :: SecretKey
    , _cbcMerklePath :: MerkleProof ByteString
    , _cbcIndex      :: Int
    }
    deriving (Eq, Show, Generic)

instance Serialise TxId
instance Serialise Tx

type Witness = PersonalisedProof TxId Tx

makeLenses ''Tx

check
    ::  ( HasPrism Proofs Witness
        , HasPrism Proofs TxId
        , HasGetter Crypto.PublicKey Address
        )
    =>  PreValidator Exceptions Ids Proofs Values ctx
check =
    PreValidator $ \StateTx { txProof } -> do
        Authenticated buyer _ Tx
            { _cbcContractID = cid
            , _cbcSecretKey  = _sk
            , _cbcEncrypted  = _chunk
            , _cbcMerklePath = _mpath
            , _cbcIndex      = _index
            } _txFees
            <- authenticate @TxId txProof

        _ <- accessContractAsBuyerAtStage buyer cid Transmitted

        -- All the work here goes inside the expander, where the choice
        -- of money destination is made.

        return ()

expand
    ::  ( HasPrism Proofs Witness
        , HasPrism Proofs TxId
        , HasGetter Crypto.PublicKey Address
        )
    => AccountId
    -> Expand ctx Witness
expand _miner =
    SeqExpanders $ one $ Expander
        -- (error "TODO: add read prefices")
        -- (error "TODO: add write prefices")
        $ \txProof -> do
            Authenticated buyer _ Tx
                { _cbcContractID = cid
                , _cbcSecretKey  = _sk
                , _cbcEncrypted  = _chunk
                , _cbcMerklePath = _mpath
                , _cbcIndex      = _index
                } _txFees
                    <- authenticate @TxId (inj txProof)

            contract <- accessContractAsBuyerAtStage buyer cid Transmitted

