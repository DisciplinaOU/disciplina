
module Dscp.Snowdrop.Contracts.Stages.BrokenChunk where

import Dscp.Crypto as Crypto
import Dscp.Snowdrop.Contracts.Contract
import Dscp.Snowdrop.Contracts.Entities
import Dscp.Snowdrop.Contracts.Util

data ContractBrokenChunkTxId = ContractBrokenChunkTxId
    deriving (Eq, Ord, Show, Generic)

data ContractBrokenChunk = ContractBrokenChunk
    { _cbcContractID :: ContractID
    , _cbcEncrypted  :: ByteString
    , _cbcSecretKey  :: SecretKey
    , _cbcMerklePath :: MerkleProof ByteString
    , _cbcIndex      :: Int
    }
    deriving (Eq, Show, Generic)

instance Serialise ContractBrokenChunkTxId
instance Serialise ContractBrokenChunk

makeLenses ''ContractBrokenChunk

checkBuyerFoundBrokenChunk
    ::  ( HasPrism Proofs (PersonalisedProof ContractBrokenChunkTxId ContractBrokenChunk)
        , HasPrism Proofs ContractBrokenChunkTxId
        , HasGetter Crypto.PublicKey Address
        )
    =>  PreValidator Exceptions Ids Proofs Values ctx
checkBuyerFoundBrokenChunk =
    PreValidator $ \StateTx { txProof } -> do
        Authenticated buyer _ ContractBrokenChunk
            { _cbcContractID = cid
            , _cbcSecretKey  = _sk
            , _cbcEncrypted  = _chunk
            , _cbcMerklePath = _mpath
            , _cbcIndex      = _index
            } _txFees
            <- authenticate @ContractBrokenChunkTxId txProof

        _ <- accessContractAsBuyer buyer cid

        -- All the work here goes inside the expander, where the choice
        -- of money destination is made.

        return ()
