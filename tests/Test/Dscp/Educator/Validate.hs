module Test.Dscp.Educator.Validate where

import Test.Common

import Dscp.Core (ATGDelta (..), CourseId (..), Grade (..))
import Dscp.Crypto (PublicKey, SecretKey, fromFoldable, getMerkleRoot, hash, sign)
import Dscp.Educator ()

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

mkPrivateTxAux :: PublicKey -- ^ Public key used to create student addr
               -> (PublicKey, SecretKey) -- ^ Key pair used to create submission witness
               -> (PublicKey, SecretKey) -- ^ Key pair used to create transaction witness
               -> PrivateTxAux
mkPrivateTxAux sAddrKey submissionWitness (txWitnessPubKey, txWitnessPrivKey) =
    let tx = mkPrivateTx courseCompScience1 B sAddrKey submissionWitness
    in PrivateTxAux { _ptaTx = tx
                    , _ptaWitness = mkWitness tx
                    }
  where mkWitness tx = PkWitness { _ptwKey = txWitnessPubKey
                                 , _ptwSig = sign txWitnessPrivKey (hash tx)
                                 }

-- | Educator 'k' grade student a an B in course Computer science
-- transaction is signed by student a
tx1 :: PrivateTxAux
tx1 = mkPrivateTxAux studentAPubKey educatorKKeyPair studentAKeyPair

-- | Educator 'k' grade student a an B in course Computer science
-- transaction is signed by student b
tx2 :: PrivateTxAux
tx2 = mkPrivateTxAux studentAPubKey educatorKKeyPair (studentAPubKey, studentBPrivKey)

tx3 :: PrivateTxAux
tx3 = mkPrivateTxAux studentAPubKey educatorKKeyPair (studentBPubKey, studentAPrivKey)

tx4 :: PrivateTxAux
tx4 = mkPrivateTxAux studentBPubKey educatorKKeyPair (studentBPubKey, studentBPrivKey)

tx5 :: PrivateTxAux
tx5 = mkPrivateTxAux studentAPubKey educatorKKeyPair (studentBPubKey, studentBPrivKey)

tx6 :: PrivateTxAux
tx6 = mkPrivateTxAux studentBPubKey
                     (educatorJPubKey, educatorKPrivKey)
                     (studentBPubKey, studentBPrivKey)

spec_ValidateBlock :: Spec
spec_ValidateBlock = describe "Validate private block" $ do
    it "can validate valid block" $ do
        validate (mkBlock [tx1]) `shouldBe` Right ()
        validate (mkBlock [tx1, tx4]) `shouldBe` Right ()
    it "do not validate non valid block" $ do
        validate (mkBlock [tx2]) `shouldBe` Left ["Tx signature not valid"]
        validate (mkBlock [tx3]) `shouldBe` Left ["Tx signature not valid", "Tx public key missmatch"]
        validate (mkBlock [tx5]) `shouldBe` Left ["Tx public key missmatch"]
        validate (mkBlock [tx6]) `shouldBe` Left ["Tx public key missmatch"]
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



