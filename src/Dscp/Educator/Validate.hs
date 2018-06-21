
-- | Validation of private block

module Dscp.Educator.Validate
       ( validatePrivateBlk
       ) where

import Universum

import Data.Text.Buildable (build)
import Serokell.Util.Verify (verResToMonadError, verifyGeneric)

import Dscp.Core (Address (..), SignedSubmission (..), Submission (..),
                  SubmissionWitness (..))
import Dscp.Crypto (MerkleSignature, fromFoldable, getMerkleRoot, hash, verify)
import Dscp.Educator.Block (PrivateBlock (..), PrivateBlockHeader (..), pbBody, pbbTxs)
import Dscp.Educator.Serialise ()
import Dscp.Educator.Txs (PrivateTx (..))

import qualified Text.Show
import qualified Data.Text.Buildable ()


-- | Exceptions during validation
data ValidationFailure
    = MerkleSignatureMismatch (MerkleSignature PrivateTx) (MerkleSignature PrivateTx)
      -- ^ Header merkle tree root does not match
      -- body merkle tree root
    | TxPublicKeyMismatch
      -- ^ Witness public key do not match transaction address
    | TxSignatureMismatch
      -- ^ Witness signature do not match transaction submission signature

instance Show ValidationFailure where
    show e = toString . pretty $ e

instance Buildable ValidationFailure where
    build (MerkleSignatureMismatch expected got) =
      "Merkle tree root signature mismatch, expected " `mappend` show expected
      `mappend` " got " `mappend` show got
    build (TxPublicKeyMismatch key addr) =
      "Tx public key mismatch. PublicKey was " `mappend` show key
    build (TxSignatureMismatch) = "Tx signature mismatch"

-- | Validate private block.
-- Private block is valid iff
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
          [(_pbhBodyProof == merkleRoot, show (MerkleSignatureMismatch _pbhBodyProof merkleRoot))]

        validateTx (PrivateTx {..}) =
          let submission = ssSubmission _ptxSignedSubmission
              witnessKey = _swKey (ssWitness _ptxSignedSubmission)
              witnessSign = _swSig (ssWitness _ptxSignedSubmission)
              txAddrHash = addrHash (sStudentId submission)
              witnessSigValid = verify witnessKey (hash submission) witnessSign
              witnessKeyValid = hash witnessKey == txAddrHash
          in [(witnessKeyValid, show (TxPublicKeyMismatch witnessKey txAddrHash))
             ,(witnessSigValid, show TxSignatureMismatch)]
