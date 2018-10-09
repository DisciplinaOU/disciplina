-- | Arbitary instances for crypto.

module Dscp.Crypto.Arbitrary () where

import Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import Dscp.Util.Test

import Dscp.Crypto.ByteArray
import Dscp.Crypto.Encrypt
import Dscp.Crypto.Hash
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

instance Arbitrary a => Arbitrary (MerkleSignature a) where
    arbitrary = MerkleSignature <$> arbitrary <*> choose (0, 100)
    shrink = genericShrink

instance Arbitrary a => Arbitrary (MerkleProof a) where
    arbitrary = genericArbitrary
    shrink = genericShrink
