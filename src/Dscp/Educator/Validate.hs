
-- | Validation of private block

module Dscp.Educator.Validate
       ( validate
       ) where

import Universum

import Dscp.Crypto (hash, verify)
import Dscp.Crypto.MerkleTree (fromFoldable, getMerkleRoot)
import Dscp.Educator.Block (PrivateBlock, pbBody, pbHeader, pbbTxs, pbhBodyProof)
import Dscp.Educator.Serialise ()
import Dscp.Educator.Txs (PrivateTxAux (..), PrivateTxWitness (..))

-- | Validate private block.
-- Block is valid if header matches body
-- and all transactions have valid signatures.
validate :: PrivateBlock -> Bool
validate pb =
    let txs = pb ^. pbBody . pbbTxs
        merkleRoot = getMerkleRoot (fromFoldable txs)
        headerProof = pb ^. pbHeader . pbhBodyProof
        headerProofValid = headerProof == merkleRoot
        txsSigProofValid = all (==True) (map validateTx txs)
    in headerProofValid && txsSigProofValid
  where validateTx :: PrivateTxAux -> Bool
        validateTx (PrivateTxAux {..}) =
          let txProofSig = _ptwSig _ptaWitness
              txProofKey = _ptwKey _ptaWitness
              tx = hash _ptaTx
          in verify txProofKey tx txProofSig
