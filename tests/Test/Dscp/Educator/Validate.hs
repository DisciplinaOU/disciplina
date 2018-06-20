module Test.Dscp.Educator.Validate where

import Test.Common

import Dscp.Core (ATGDelta (..), CourseId (..), Grade (..))
import Dscp.Crypto (AbstractPK (..), AbstractSK (..), PublicKey, SecretKey,
                    getMerkleRoot, fromFoldable)
import Dscp.Educator (PrivateBlock (..), PrivateBlockBody (..), PrivateBlockHeader (..),
                      PrivateTx (..), genesisHeaderHash, validatePrivateBlk)

import System.IO.Unsafe (unsafePerformIO)
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.Map.Strict as M

courseCompScience1 :: CourseId
courseCompScience1 = CourseId 3

studentAPubKey, studentBPubKey,
  educatorJPubKey, educatorKPubKey :: PublicKey
studentAPubKey = mkPubKey 'a'
studentBPubKey = mkPubKey 'b'
educatorJPubKey = mkPubKey 'j'
educatorKPubKey = mkPubKey 'k'

studentAPrivKey, studentBPrivKey,
  educatorJPrivKey, educatorKPrivKey :: SecretKey
studentAPrivKey = mkPrivKey 'a'
studentBPrivKey = mkPrivKey 'b'
educatorJPrivKey = mkPrivKey 'j'
educatorKPrivKey = mkPrivKey 'k'

educatorKKeyPair, studentAKeyPair :: (PublicKey, SecretKey)
educatorKKeyPair = mkKeyPair 'k'
studentAKeyPair = mkKeyPair 'a'

-- | Educator 'k' grade student a an B in course Computer science
-- transaction is signed by student a
tx1, tx2, tx3, tx4 :: PrivateTx
tx1 = mkPrivateTx courseCompScience1 B studentAPubKey studentAKeyPair

-- | Educator 'k' grade student a an B in course Computer science
-- transaction is signed by student b
tx2 = mkPrivateTx courseCompScience1 B studentAPubKey (studentAPubKey, studentBPrivKey)

tx3 = mkPrivateTx courseCompScience1 B studentAPubKey (studentBPubKey, studentAPrivKey)

tx4 = mkPrivateTx courseCompScience1 B studentBPubKey (studentBPubKey, studentBPrivKey)

-- | unsafePerformIO create bunch of PrivateTx.
-- Use map instead of replicate here to force
-- unique key pairs
txsValid :: [PrivateTx]
txsValid = map generateKeyPair [1..(100 :: Int)]
  where generateKeyPair _ =
          let key = unsafePerformIO Ed25519.generateSecretKey
              kp@(pubKey, _) = (AbstractPK (Ed25519.toPublic key), AbstractSK key)
          in mkPrivateTx courseCompScience1 B pubKey kp

spec_ValidateBlock :: Spec
spec_ValidateBlock = describe "Validate private block" $ do
    it "can validate valid block" $ do
        validatePrivateBlk (mkBlock [tx1]) `shouldBe` Right ()
        validatePrivateBlk (mkBlock [tx1, tx4]) `shouldBe` Right ()
        validatePrivateBlk (mkBlock txsValid) `shouldBe` Right ()
    it "do not validate non valid block" $ do
        validatePrivateBlk (mkBlock [tx2]) `shouldBe` Left ["Tx signature not valid"]
        validatePrivateBlk (mkBlock [tx3]) `shouldBe` Left ["Tx public key missmatch", "Tx signature not valid"]
  where mkBlock txs = PrivateBlock
          { _pbHeader = mkBlockHeader txs
          , _pbBody = mkBlockBody txs
          }
        mkBlockHeader txs = PrivateBlockHeader
          { _pbhPrevBlock = genesisHeaderHash
          , _pbhBodyProof = mkBodyProof txs
          , _pbhAtgDelta = mkATGDelta
          }
        mkBlockBody txs = PrivateBlockBody
          { _pbbTxs = txs
          }
        mkBodyProof txs = getMerkleRoot (fromFoldable txs)
        mkATGDelta = ATGDelta M.empty
