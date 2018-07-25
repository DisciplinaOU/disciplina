{-# LANGUAGE QuasiQuotes #-}

-- | Validation of private block

module Dscp.Educator.BlockValidation
       ( BlockValidationFailure (..)
       , SubmissionValidationFailure (..)
       , validateSubmission
       , validatePrivateBlk
       ) where

import Control.Lens (to)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Text.Buildable (build)

import Dscp.Core (Address (..), SignedSubmission (..), Submission (..), SubmissionSig, sStudentId,
                  ssSubmission, ssWitness, swKey, swSig)
import Dscp.Crypto (Hash, MerkleSignature, PublicKey, fromFoldable, getMerkleRoot, hash, verify)
import Dscp.Educator.Block (PrivateBlock (..), PrivateBlockHeader (..), _pbBody, _pbbTxs)
import Dscp.Educator.Txs (PrivateTx (..))
import Text.InterpolatedString.Perl6 (qc)

import qualified Data.Text.Buildable ()
import qualified Text.Show

import Dscp.Util (mappendLefts)

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
    deriving (Eq, Show)

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

deriveJSON defaultOptions ''SubmissionValidationFailure

verifyGeneric :: [(Bool, e)] -> Either [e] ()
verifyGeneric checks = case filter ((==False) . fst) checks of
    [] -> Right ()
    x  -> Left  (map snd x)

-- | Validate signed submission.
-- Signed submission is valid iff
-- 1. It has valid signatures
-- 2. Signature public key corresponds to student public key
--    for given submission
validateSubmission
    :: SignedSubmission
    -> Either [SubmissionValidationFailure] ()
validateSubmission ss =
    let submission = ss^.ssSubmission
        witnessKey = ss^.ssWitness.swKey
        witnessSign = ss^.ssWitness.swSig
        subAddrHash = submission^.sStudentId.to addrHash
        witnessSigValid = verify witnessKey (hash submission) witnessSign
        witnessKeyValid = hash witnessKey == subAddrHash
    in verifyGeneric
        [ (witnessKeyValid,
            SubmissionPublicKeyMismatch { svfExpectedPubKey = subAddrHash
                                        , svfActualPubKey   = hash witnessKey
                                        }
          )
        , (witnessSigValid,
            SubmissionSignatureMismatch { svfSubmissionHash   = hash submission
                                        , svfSubmissionSig    = witnessSign
                                        , svfSubmissionSigKey = witnessKey
                                        }
          )
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
        headerValid = verifyGeneric headerVer
        subValid = foldl' (\acc sub -> validateSub sub `mappendLefts` acc)
                   pass subs
    in headerValid `mappendLefts` subValid
  where
    validateHeader (PrivateBlockHeader {..}) merkleRoot =
        [ (_pbhBodyProof == merkleRoot,
          MerkleSignatureMismatch { bvfExpectedSig     = _pbhBodyProof
                                  , bvfActualMerkleSig =  merkleRoot
                                  }
          )
        ]
    validateSub sub =
        first (map SubmissionInvalid) $
        validateSubmission (_ptSignedSubmission sub)
