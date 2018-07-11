module Test.Dscp.Crypto.Instances () where

import Test.Common

import Dscp.Crypto (AbstractHash, AbstractPK, AbstractSK, AbstractSig, Encrypted, FromByteArray,
                    HashFunc (..), PassPhrase, SignatureScheme (..), encrypt, mkPassPhrase)

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
