{-# LANGUAGE QuasiQuotes #-}

-- | Validation of private block and private transactions.

module Dscp.Core.Validation.Educator
       ( BlockValidationFailure (..)
       , SubmissionValidationFailure (..)
       , CertificationValidationFailure (..)
       , WrongSubmissionSignature (..)
       , _FakeSubmissionSignature
       , _SubmissionSignatureInvalid
       , validateSubmission
       , verifyStudentSubmission
       , verifyCertificate
       , validatePrivateBlk
       ) where

import Control.Lens (makePrisms, to)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Text.Buildable (build)
import qualified Data.Text.Buildable ()
import Fmt (listF, (+|))
import Text.InterpolatedString.Perl6 (qc)
import qualified Text.Show

import Dscp.Core.Foundation
import Dscp.Crypto
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

    | CertificateInvalid
        { bvfCertificationFailure :: !CertificationValidationFailure }

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

data CertificationValidationFailure
    = CertificationSignatureMismatch
      { cvfCertificationHash   :: !(Hash CertificateGrade)
      , cvfCertificationSigKey :: !PublicKey
      , cvfCertificationSig    :: !(Signature (Hash CertificateGrade))
      }
    deriving (Eq, Show)

-- | Any issues with submission signature content.
data WrongSubmissionSignature
    = FakeSubmissionSignature
      -- ^ Signature doesn't match the student who performs the request.
    | SubmissionSignatureInvalid [SubmissionValidationFailure]
      -- ^ Submission is invalid on itself.
    deriving (Eq, Show, Generic)

instance Exception WrongSubmissionSignature

instance Show BlockValidationFailure where
    show = toString . pretty

instance Buildable BlockValidationFailure where
    build MerkleSignatureMismatch {..} =
      [qc|Merkle tree root signature mismatch. Expected {bvfExpectedSig} got {bvfActualMerkleSig}|]
    build (CertificateInvalid x) = build x
    build (SubmissionInvalid  x) = build x

instance Buildable CertificationValidationFailure where
    build CertificationSignatureMismatch {..} =
      mconcat [ [qc|Certification signature mismatch.|]
              , [qc| Certificate data {cvfCertificationHash} with signature {cvfCertificationSig}|]
              , [qc| does not correspond to public key {cvfCertificationSigKey}|]
              ]

instance Buildable SubmissionValidationFailure where
    build SubmissionPublicKeyMismatch {..} =
      mconcat [ [qc|Submission public key address mismatch. |]
              , [qc|Expected {svfExpectedPubKey} got {svfActualPubKey}|]
              ]
    build SubmissionSignatureMismatch {..} =
      mconcat [ [qc|Submission signature mismatch.|]
              , [qc| Submission data {svfSubmissionHash} with signature {svfSubmissionSig}|]
              , [qc| does not correspond to public key {svfSubmissionSigKey}|]
              ]

instance Buildable WrongSubmissionSignature where
    build FakeSubmissionSignature =
        "Submission signature does not belong to the student who provided it"
    build (SubmissionSignatureInvalid failures) =
        "Signature validation has failed: "+|listF failures

makePrisms ''WrongSubmissionSignature
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

verifyCertificate
    :: SignedCertificateGrade
    -> Either [CertificationValidationFailure] ()
verifyCertificate (SignedCertificateGrade cg pk sig) =
    let cgHash = hash cg
    in
        if verify pk cgHash sig
        then Right ()
        else Left $ [CertificationSignatureMismatch cgHash pk sig]

-- | Checks that
-- 1. 'SignedSubmission' is valid;
-- 2. It was actually signed by a student who makes a request.
verifyStudentSubmission
    :: Student
    -> SignedSubmission
    -> Either WrongSubmissionSignature ()
verifyStudentSubmission author ss = do
    let signatureAuthor = _sStudentId (_ssSubmission ss)
    unless (signatureAuthor == author) $
        Left FakeSubmissionSignature
    validateSubmission ss
        & first SubmissionSignatureInvalid

-- | Validate private block.
-- Private block is valid iff
-- 1. Header merkle signature matches computed body merkle signature
-- 2. All submission in block have valid signatures
-- 3. All submission signature public keys corresponds to student public key
--    for given submission
validatePrivateBlk :: PrivateBlock -> Either [BlockValidationFailure] ()
validatePrivateBlk pb =
    let subs        = _pbbTxs (_pbBody pb)
        merkleRoot  = getMerkleRoot (fromFoldable subs)
        headerVer   = validateHeader (_pbHeader pb) merkleRoot
        headerValid = verifyGeneric headerVer
        ptxValid    =
            foldl' (\acc ptx -> validatePtx ptx
              `mappendLefts` acc)
                pass subs
    in headerValid `mappendLefts` ptxValid
  where
    validateHeader PrivateBlockHeader {..} merkleRoot =
        [ (_pbhBodyProof == merkleRoot,
          MerkleSignatureMismatch
            { bvfExpectedSig     = _pbhBodyProof
            , bvfActualMerkleSig =  merkleRoot
            }
          )
        ]
    validatePtx = \case
        PrivateTxGrade pg ->
          first (fmap SubmissionInvalid) $
            validateSubmission (pg^.ptSignedSubmission)

        PrivateTxCertification cert ->
          first (fmap CertificateInvalid) $
            verifyCertificate (cert^.pcGrade)
