
-- | Validation of private block

module Dscp.Educator.Validate
       ( validatePrivateBlk
       ) where

import Universum

import Serokell.Util.Verify (verResToMonadError, verifyGeneric)

import Dscp.Core (Address (..), SignedSubmission (..), Submission (..),
                  SubmissionWitness (..))
import Dscp.Crypto (fromFoldable, getMerkleRoot, hash, verify)
import Dscp.Educator.Block (PrivateBlock (..), PrivateBlockHeader (..), pbBody, pbbTxs)
import Dscp.Educator.Serialise ()
import Dscp.Educator.Txs (PrivateTx (..))

-- | Validate private block.
-- Block is valid if
-- 1. Header matches body
-- 2. All transactions have valid signatures
-- 3. Transaction signature public key correspond to student public key
validatePrivateBlk :: PrivateBlock -> Either [Text] ()
validatePrivateBlk pb =
    let txs = pb ^. pbBody . pbbTxs
        merkleRoot = getMerkleRoot (fromFoldable txs)
        headerVer = validateTxHeader (_pbHeader pb) merkleRoot
        blockValid = verifyGeneric (headerVer <> concatMap validateTx txs)
    in verResToMonadError toList blockValid
  where validateTxHeader (PrivateBlockHeader {..}) merkleRoot =
          [(_pbhBodyProof == merkleRoot, "Header signature missmatch")]

        validateTx (PrivateTx {..}) =
          let submission = ssSubmission _ptxSignedSubmission
              witnessKey = _swKey (ssWitness _ptxSignedSubmission)
              witnessSign = _swSig (ssWitness _ptxSignedSubmission)
              txAddrHash = addrHash (sStudentId submission)
              witnessSigValid = verify witnessKey (hash submission) witnessSign
              witnessKeyValid = hash witnessKey == txAddrHash
          in [(witnessKeyValid, "Tx public key missmatch")
             ,(witnessSigValid, "Tx signature not valid")]
