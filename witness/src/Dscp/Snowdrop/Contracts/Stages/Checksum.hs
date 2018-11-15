
module Dscp.Snowdrop.Contracts.Stages.Checksum where

import Dscp.Snowdrop.Contracts.Account

data ContractChecksum = ContractChecksum
    { _ccContractID :: ContractID
    , _ccChecksum   :: MerkleSignature ByteString
    , _ccStage      :: Stage
    }

makeLenses ''ContractChecksum

checkBuyerCalculatedChecksum =
    PreValidator $ \StateTx { txProof } -> do
        Authenticated buyer _ ContractChecksum
            { _ccContractID = cid
            , _ccChecksum   = signature
            , _ccStage      = stage
            } txFees
            <- authenticate @_ txProof

        contract     <- getContract cid
        theSignature <- getSignatureOfPublication (contract^.caPublication)

        () <- check (signature /= theSignature && stage == Cancelled)   CancellingOnCorrectSig
        () <- check (signature == theSignature && stage == Transmitted) NotCancellingOnInCorrectSig

        return ()

