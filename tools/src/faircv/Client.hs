module Client where

import Dscp.Core
import Dscp.Crypto
import Dscp.Educator.Web.Student
import Dscp.Educator.Web.Types
import Dscp.Util
import Dscp.Util.Aeson

-- | Get all proofs since given time.
getProofs :: StudentApiClient -> IO [MerkleProof PrivateTx]
getProofs _ = do
    rawProofs <- sGetProofs sc Nothing False
    let zipProof BlkProofInfo {..} = mergeProofAndData
            (unEncodeSerialised bpiMtreeSerialized)
            bpiTxs
    nothingToThrow (IOException "Invalid proofs") $
        mapM zipProof rawProofs
