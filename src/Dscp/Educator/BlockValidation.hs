{-# LANGUAGE QuasiQuotes #-}
-- | Validation of private block

module Dscp.Educator.BlockValidation
       ( BlockValidationFailure (..)
       , SubmissionValidationFailure (..)
       , validatePrivateBlk
       ) where

import Universum

import Control.Lens (to)
import Data.Text.Buildable (build)

import Dscp.Core (Address (..), Submission (..), SubmissionSig, ssSubmission,
                  ssWitness, sStudentId, swKey, swSig)
import Dscp.Crypto (Hash, PublicKey, MerkleSignature, fromFoldable, getMerkleRoot, hash, verify)
import Dscp.Educator.Block (PrivateBlock (..), PrivateBlockHeader (..), _pbBody, _pbbTxs)
import Dscp.Educator.Serialise ()
import Dscp.Educator.Txs (PrivateTx (..))
import Text.InterpolatedString.Perl6 (qc)

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
      [qc|Merkle tree root signature mismatch. Expected {bvfExpectedSig} got {bvfActualMerkleSig}|]
    build (SubmissionInvalid x) = build x

instance Buildable SubmissionValidationFailure where
    build (SubmissionPublicKeyMismatch {..}) =
      mconcat [ [qc|Submission public key address mismatch. |]
              , [qc|Expected {svfExpectedPubKey} got {svfActualPubKey}|]
              ]
    build (SubmissionSignatureMismatch {..}) =
      mconcat [ [qc|Submission signature mismatch.|]
              , [qc| Submission data {svfSubmissionHash} with signature {svfSubmissionSig}|]
              , [qc| does not correspond to public key {svfSubmissionSigKey}|]
              ]

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
  where
    validateHeader (PrivateBlockHeader {..}) merkleRoot =
        [ (_pbhBodyProof == merkleRoot,
          MerkleSignatureMismatch { bvfExpectedSig     = _pbhBodyProof
                                  , bvfActualMerkleSig =  merkleRoot
                                  }
          )
        ]
    validateSub (PrivateTx {..}) =
        let submission = _ptSignedSubmission^.ssSubmission
            witnessKey = _ptSignedSubmission^.ssWitness.swKey
            witnessSign = _ptSignedSubmission^.ssWitness.swSig
            subAddrHash = submission^.sStudentId.to addrHash
            witnessSigValid = verify witnessKey (hash submission) witnessSign
            witnessKeyValid = hash witnessKey == subAddrHash
        in [ (witnessKeyValid,
              SubmissionInvalid { bvfSubmissionFailure =
                                    SubmissionPublicKeyMismatch { svfExpectedPubKey = subAddrHash
                                                                , svfActualPubKey   = hash witnessKey
                                                                }
                                }
             )
           , (witnessSigValid,
              SubmissionInvalid { bvfSubmissionFailure =
                                    SubmissionSignatureMismatch { svfSubmissionHash   = hash submission
                                                                , svfSubmissionSig    = witnessSign
                                                                , svfSubmissionSigKey = witnessKey
                                                                }
                                }
             )
           ]
