
-- | Validation of private block

module Dscp.Educator.Validate
       ( validate
       ) where

import Universum

import Control.Monad.Except (MonadError)
import Serokell.Util.Verify (verResToMonadError, verifyGeneric)

import Dscp.Core (Address (..), SignedSubmission (..), Submission (..))
import Dscp.Crypto (fromFoldable, getMerkleRoot, hash, verify)
import Dscp.Educator.Block (PrivateBlock (..), PrivateBlockHeader (..), pbBody, pbbTxs)
import Dscp.Educator.Serialise ()
import Dscp.Educator.Txs (PrivateTx (..), PrivateTxAux (..), PrivateTxWitness (..))

-- | Validate private block.
-- Block is valid if
-- 1. Header matches body
-- 2. All transactions have valid signatures
-- 3. Transaction signature public key correspond to student public key
validate :: (MonadError [Text] m) => PrivateBlock -> m ()
validate pb =
    let txs = pb ^. pbBody . pbbTxs
        merkleRoot = getMerkleRoot (fromFoldable txs)
        headerVer = validateTxHeader (_pbHeader pb) merkleRoot
        blockValid = verifyGeneric (headerVer <> concatMap validateTx txs)
    in verResToMonadError toList blockValid
  where validateTxHeader (PrivateBlockHeader {..}) merkleRoot =
          [(_pbhBodyProof == merkleRoot, "Header signature missmatch")]

        validateTx (PrivateTxAux {..}) =
          let txProofSig = _ptwSig _ptaWitness
              txProofKey = _ptwKey _ptaWitness
              tx = hash _ptaTx
              txAddrHash = addrHash (sStudentId (ssSubmission (_ptxSignedSubmission _ptaTx)))
          in [(verify txProofKey tx txProofSig, "Tx signature not valid")
             ,(hash txProofKey == txAddrHash, "Tx public key missmatch")
             ]
