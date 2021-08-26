module Test.Dscp.Core.Validation.Educator where

import qualified Data.Map.Strict as M
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec

import Dscp.Core
import Dscp.Crypto
import Dscp.Util

courseCompScience1 :: Id Course
courseCompScience1 = Course 3

studentAPubKey, studentBPubKey :: PublicKey
studentAPubKey = mkPubKey 'a'
studentBPubKey = mkPubKey 'b'

studentAPrivKey, studentBPrivKey :: SecretKey
studentAPrivKey = mkPrivKey 'a'
studentBPrivKey = mkPrivKey 'b'

studentAKeyPair :: (PublicKey, SecretKey)
studentAKeyPair = mkKeyPair 'a'

-- | Educator 'k' grade student a an B in course Computer science
-- transaction is signed by student a
tx1, tx2, tx3, tx4 :: PrivateTx
tx1 = mkTestPrivateTx courseCompScience1 gB studentAPubKey studentAKeyPair

-- | Educator 'k' grade student a an B in course Computer science
-- transaction is signed by student b
tx2 = mkTestPrivateTx courseCompScience1 gB studentAPubKey (studentAPubKey, studentBPrivKey)

tx3 = mkTestPrivateTx courseCompScience1 gB studentAPubKey (studentBPubKey, studentAPrivKey)

tx4 = mkTestPrivateTx courseCompScience1 gB studentBPubKey (studentBPubKey, studentBPrivKey)

-- | unsafePerformIO create bunch of PrivateTx.
-- Use map instead of replicate here to force
-- unique key pairs
txsValid :: [PrivateTx]
txsValid = map generateKeyPair [1..(100 :: Int)]
  where generateKeyPair _ =
          let key = unsafePerformIO ssGenSecret
              kp@(pubKey, _) = (ssToPublic key, key)
          in mkTestPrivateTx courseCompScience1 gB pubKey kp

spec_ValidateBlock :: Spec
spec_ValidateBlock = describe "Validate private block" $ do
    it "can validate valid block" $ do
        validatePrivateBlk (mkBlock [tx1]) `shouldBe` Right ()
        validatePrivateBlk (mkBlock [tx1, tx4]) `shouldBe` Right ()
        validatePrivateBlk (mkBlock txsValid) `shouldBe` Right ()
    it "do not validate non-valid transaction signatures " $ do
        validatePrivateBlk (mkBlock [tx2]) `shouldBe`
          Left [SubmissionInvalid
                 (SubmissionSignatureMismatch { svfSubmissionHash = hashTxSub tx2
                                              , svfSubmissionSig = getTxSig tx2
                                              , svfSubmissionSigKey = getTxKey tx2
                                              })]
        validatePrivateBlk (mkBlock [tx3]) `shouldBe`
          Left [SubmissionInvalid
                 (SubmissionPublicKeyMismatch { svfExpectedPubKey = hash studentAPubKey
                                              , svfActualPubKey = hash (getTxKey tx3)
                                              })
               ,SubmissionInvalid
                 (SubmissionSignatureMismatch { svfSubmissionHash = hashTxSub tx3
                                              , svfSubmissionSig = getTxSig tx3
                                              , svfSubmissionSigKey = getTxKey tx3
                                              })]
    it "do not validate non-valid merkle root " $ do
        let block = mkBlock txsValid
        validatePrivateBlk (replaceMerkleRoot block [tx1, tx2, tx4]) `shouldBe`
          Left [MerkleSignatureMismatch { bvfExpectedSig = mkBodyProof [tx1, tx2, tx4]
                                        , bvfActualMerkleSig = _pbhBodyProof (_pbHeader block)
                                        }]
  where
    educatorAddr = mkAddr $ mkPubKey 'z'
    getTxKey tx = tx^.ptSignedSubmission.ssWitness.swKey
    getTxSig tx = tx^.ptSignedSubmission.ssWitness.swSig
    hashTxSub tx = hash $ tx^.ptSignedSubmission.ssSubmission
    mkBlock txs = PrivateBlock
        { _pbHeader = mkBlockHeader txs
        , _pbBody = mkBlockBody txs
        }
    mkBlockHeader txs = PrivateBlockHeader
        { _pbhPrevBlock = genesisHeaderHash educatorAddr
        , _pbhBodyProof = mkBodyProof txs
        , _pbhAtgDelta = mkATGDelta
        }
    mkBlockBody txs = PrivateBlockBody
        { _pbbTxs = txs }
    mkBodyProof txs = getMerkleRoot (fromFoldable txs)
    mkATGDelta = ATGDelta M.empty
    replaceMerkleRoot :: PrivateBlock -> [PrivateTx] -> PrivateBlock
    replaceMerkleRoot block txs =
        block { _pbHeader = (_pbHeader block) { _pbhBodyProof = mkBodyProof txs } }
