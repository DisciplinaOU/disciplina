module Test.Dscp.Crypto.Instances () where

import Test.Common

import Dscp.Crypto (AbstractSK, Encrypted, FromByteArray, PassPhrase, SignatureScheme, encrypt,
                    genSecretKey, mkPassPhrase)

instance (SignatureScheme ss, FromByteArray (AbstractSK ss)) =>
         Arbitrary (AbstractSK ss) where
    arbitrary = genSecureRandom genSecretKey

instance Arbitrary PassPhrase where
    arbitrary = arbitrary `suchThatMap` (rightToMaybe . mkPassPhrase)

instance (Arbitrary ba, FromByteArray ba) =>
         Arbitrary (Encrypted ba) where
    arbitrary = encrypt <$> arbitrary <*> arbitrary
