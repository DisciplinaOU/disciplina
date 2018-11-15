
module Dscp.Snowdrop.Contracts.Stages.BrokenChunk where

import Dscp.Snowdrop.Contracts.Account

data ContractBrokenChunk = ContractBrokenChunk
    { _cbcContractID :: ContractID
    , _cbcEncrypted  :: ByteString
    , _cbcSecretKey  :: SecretKey
    , _cbcMerklePath :: MerkleProof ByteString
    , _cbcIndex      :: Int
    }

makeLenses ''ContractBrokenChunk

checkBuyerFoundBrokenChunk =
    PreValidator $ \StateTx { txProof } -> do
        Authenticated buyer _ ContractBrokenChunk
            { _cbcContractID = cid
            , _cbcSecretKey  = sk
            , _cbcEncrypted  = chunk
            , _cbcMerklePath = mpath
            , _cbcIndex      = index
            } txFees
            <- authenticate @_ txProof

        contract <- getContract cid

        () <- check (buyer == contract^.caBuyer) WrongBuyer

        -- All the work here goes inside the expander, where the choice
        -- of money destination is made.

        return ()
