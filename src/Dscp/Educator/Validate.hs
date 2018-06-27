
-- | Validation of private block

module Dscp.Educator.Validate
       ( BlockValidationFailure (..)
       , SubmissionValidationFailure (..)
       , validatePrivateBlk
       ) where

import Universum

import Data.Text.Buildable (build)

import Dscp.Core (Address (..), SignedSubmission (..), Submission (..),
                  SubmissionSig, SubmissionWitness (..))
import Dscp.Crypto (Hash, PublicKey, MerkleSignature, fromFoldable, getMerkleRoot, hash, verify)
import Dscp.Educator.Block (PrivateBlock (..), PrivateBlockHeader (..), _pbBody, _pbbTxs)
import Dscp.Educator.Serialise ()
import Dscp.Educator.Txs (PrivateTx (..))

import qualified Text.Show
import qualified Data.Text.Buildable ()


-- | Block validation failures
data BlockValidationFailure
    = -- | Header merkle tree root does not match body merkle tree root
      MerkleSignatureMismatch
        { bvfExpectedSig     :: !(MerkleSignature PrivateTx)
        , bvfActualMerkleSig :: !(MerkleSignature PrivateTx) }
      -- | Submission failure
    | SubmissionInvalid
        { bvfSubmissionFailure :: !SubmissionValidationFailure }
    deriving (Eq)

-- | Submission validation failures
data SubmissionValidationFailure
    = -- | Witness public key do not match submission address
      SubmissionPublicKeyMismatch
        { svfExpectedPubKey :: !(Hash PublicKey)
        , svfActualPubKey   :: !(Hash PublicKey) }
      -- | Witness signature do not match submission signature
    | SubmissionSignatureMismatch
        { svfSubmissionHash   :: !(Hash Submission)
        , svfSubmissionSigKey :: !PublicKey
        , svfSubmissionSig    :: !SubmissionSig }
    deriving (Eq)

instance Show BlockValidationFailure where
    show = toString . pretty

instance Buildable BlockValidationFailure where
    build (MerkleSignatureMismatch {..}) =
      "Merkle tree root signature mismatch. Expected " `mappend` show bvfExpectedSig
      `mappend` " got " `mappend` show bvfActualMerkleSig
    build (SubmissionInvalid (SubmissionPublicKeyMismatch {..})) =
      "Submission public key address mismatch. Expected " `mappend` show svfExpectedPubKey `mappend`
      " got " `mappend` show svfActualPubKey
    build (SubmissionInvalid (SubmissionSignatureMismatch {..})) =
      "Submission signature mismatch. Submission data " `mappend` show svfSubmissionHash
      `mappend` " with signature " `mappend` show svfSubmissionSig
      `mappend` " does not correspond to public key " `mappend` show svfSubmissionSigKey

-- | Validate private block.
-- Private block is valid iff
-- 1. Header merkle signature matches computed body merkle signature
-- 2. All submission in block have valid signatures
-- 3. All submission signature public keys corresponds to student public key
--    for given submission
validatePrivateBlk :: PrivateBlock -> Either [BlockValidationFailure] ()
validatePrivateBlk pb =
    let subs =_pbbTxs (_pbBody pb)
        merkleRoot = getMerkleRoot (fromFoldable subs)
        headerVer = validateHeader (_pbHeader pb) merkleRoot
        blockValid = filter ((==False) . fst) (headerVer <> concatMap validateSub subs)
    in case blockValid of
         [] -> Right ()
         x  -> Left  (map snd x)
  where validateHeader (PrivateBlockHeader {..}) merkleRoot =
          [(_pbhBodyProof == merkleRoot,
            MerkleSignatureMismatch { bvfExpectedSig     = _pbhBodyProof
                                    , bvfActualMerkleSig =  merkleRoot
                                    })]

        validateSub (PrivateTx {..}) =
          let submission = ssSubmission _ptxSignedSubmission
              witnessKey = _swKey (ssWitness _ptxSignedSubmission)
              witnessSign = _swSig (ssWitness _ptxSignedSubmission)
              subAddrHash = addrHash (sStudentId submission)
              witnessSigValid = verify witnessKey (hash submission) witnessSign
              witnessKeyValid = hash witnessKey == subAddrHash
          in [(witnessKeyValid,
               SubmissionInvalid { bvfSubmissionFailure =
                                     SubmissionPublicKeyMismatch { svfExpectedPubKey = subAddrHash
                                                                 , svfActualPubKey   = hash witnessKey
                                                                 }})
             ,(witnessSigValid,
               SubmissionInvalid { bvfSubmissionFailure =
                                     SubmissionSignatureMismatch { svfSubmissionHash   = hash submission
                                                                 , svfSubmissionSig    = witnessSign
                                                                 , svfSubmissionSigKey = witnessKey
                                                                 }})]
