module Test.Dscp.Crypto.Instances () where

import Data.ByteArray (ByteArray)
import qualified Data.ByteString as BS
import Test.Common

import Dscp.Crypto (Encrypted, PassPhrase, encrypt, maxPassPhraseLength, minPassPhraseLength,
                    mkPassPhrase)

instance Arbitrary PassPhrase where
    arbitrary = either (error . show) identity . mkPassPhrase <$> arbitrary `suchThat`
        (\bs -> BS.length bs >= minPassPhraseLength && BS.length bs <= maxPassPhraseLength)

instance (Arbitrary ba, ByteArray ba) => Arbitrary (Encrypted ba) where
    arbitrary = encrypt <$> arbitrary <*> arbitrary
