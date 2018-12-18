module Dscp.Snowdrop.Contracts.Stages.DupedChunk where

import Dscp.Crypto as Crypto
import Dscp.Snowdrop.Contracts.Contract
import Dscp.Snowdrop.Contracts.Entities
import Dscp.Snowdrop.Contracts.Util

data ContractDupedChunkTxId = ContractDupedChunkTxId
    deriving (Eq, Ord, Show, Generic)

data ContractDupedChunk = ContractDupedChunk
    { _cdcContractID :: ContractID
    , _cdcMerklePath :: MerkleProof ByteString
    , _cdcIndexI     :: Int
    , _cdcIndexJ     :: Int
    }
    deriving (Eq, Show, Generic)

instance Serialise ContractDupedChunkTxId
instance Serialise ContractDupedChunk

makeLenses ''ContractDupedChunk

checkBuyerFoundDupedChunk
    ::  ( HasPrism Proofs (PersonalisedProof ContractDupedChunkTxId ContractDupedChunk)
        , HasPrism Proofs ContractDupedChunkTxId
        , HasGetter Crypto.PublicKey Address
        )
    =>  PreValidator Exceptions Ids Proofs Values ctx
checkBuyerFoundDupedChunk =
    PreValidator $ \StateTx { txProof } -> do
        Authenticated buyer _ ContractDupedChunk
            { _cdcContractID = cid
            , _cdcMerklePath = _ipath
            , _cdcIndexI     = _i
            , _cdcIndexJ     = _j
            } _txFees
            <- authenticate @ContractDupedChunkTxId txProof

        _ <- accessContractAsBuyerAtStage buyer cid Transmitted

        -- All the work here goes inside the expander, too.

        return ()
