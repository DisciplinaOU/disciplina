-- | Arbitary instances for crypto.

module Dscp.Crypto.Arbitrary () where

import qualified Data.Set as Set
import GHC.Exts (fromList)
import Test.QuickCheck.Arbitrary.Generic (genericShrink)

import Dscp.Util.Test

import Dscp.Crypto.ByteArray
import Dscp.Crypto.Encrypt
import Dscp.Crypto.Hash
import Dscp.Crypto.Impl (HasHash)
import Dscp.Crypto.MerkleTree
import Dscp.Crypto.Signing

----------------------------------------------------------------------------
-- Hashing
----------------------------------------------------------------------------

instance HashFunc hf => Arbitrary (AbstractHash hf a) where
    arbitrary = unsafeHashBytes <$> arbitrary @ByteString

----------------------------------------------------------------------------
-- Signatures
----------------------------------------------------------------------------

instance SignatureScheme ss => Arbitrary (AbstractSK ss) where
    arbitrary = genSecureRandom ssGenSecret

instance SignatureScheme ss => Arbitrary (AbstractPK ss) where
    arbitrary = ssToPublic <$> arbitrary

instance SignatureScheme ss => Arbitrary (AbstractSig ss a) where
    arbitrary = ssSignBytes <$> arbitrary <*> arbitrary @ByteString

----------------------------------------------------------------------------
-- Symmetric encryption
----------------------------------------------------------------------------

instance Arbitrary PassPhrase where
    arbitrary = arbitrary `suchThatMap` (rightToMaybe . mkPassPhrase)

instance (Arbitrary ba, FromByteArray ba) =>
         Arbitrary (Encrypted ba) where
    arbitrary = encrypt <$> arbitrary <*> arbitrary

----------------------------------------------------------------------------
-- Merkle tree

----------------------------------------------------------------------------

longerThan :: Container f => Int -> f -> Bool
longerThan n x = length x > n

instance Arbitrary ElementStub where
    arbitrary = pure ElementStub

instance Arbitrary (MerkleSignature a) where
    arbitrary = MerkleSignature <$> choose (0, 100) <*> arbitrary
    shrink = genericShrink

instance (HasHash a, Arbitrary a) => Arbitrary (MerkleTree a) where
    arbitrary = fromList <$> arbitrary

instance (HasHash a, Arbitrary a) => Arbitrary (MerkleNode a) where
    arbitrary = arbitrary `suchThat` longerThan 0 >>= \case
        MerkleTree node -> pure node
        _ -> error "impossible"

instance (HasHash a, Arbitrary a) => Arbitrary (MerkleProof a) where
    arbitrary = do
        tree <- MerkleTree <$> arbitrary
        let n = length tree
        idxs <- sublistOf [(0 :: Word32) .. fromIntegral n - 1]
                `suchThat` longerThan 0
        maybe (error "impossible") pure $
            mkMerkleProof tree $ Set.fromList idxs

instance (HasHash a, Arbitrary a) => Arbitrary (EmptyMerkleTree a) where
    arbitrary = getEmptyMerkleTree <$> arbitrary

instance (HasHash a, Arbitrary a) => Arbitrary (EmptyMerkleProof a) where
    arbitrary = getEmptyMerkleProof <$> arbitrary
