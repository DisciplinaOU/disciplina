module Test.Dscp.Crypto.Encrypt where

import qualified Data.ByteString as BS
import Test.Common

import Dscp.Crypto (PassPhrase, decrypt, encrypt, minPassPhraseLength, mkPassPhrase)

instance Arbitrary PassPhrase where
    arbitrary = either error identity . mkPassPhrase <$>
        arbitrary `suchThat` ((>= minPassPhraseLength) . BS.length)

spec_encryption :: Spec
spec_encryption = describe "AES encryption functions" $ do
    it "should stay the same after encryption and decryption" $ property $
        \(pp, bs :: ByteString) -> decrypt pp (encrypt pp bs) === Just bs
    it "should yield different ciphertexts for different plaintexts" $ property $
        \(pp, bs :: ByteString, bs' :: ByteString) -> bs /= bs' ==> encrypt pp bs /= encrypt pp bs'
    it "should encrypt differently for different passphrases" $ property $
        \(pp, pp', bs :: ByteString) -> pp /= pp' ==> encrypt pp bs /= encrypt pp' bs
    it "should fail when trying to decrypt a ciphertext with wrong passphrase" $ property $
        \(pp, pp', bs :: ByteString) -> pp /= pp' ==> decrypt pp (encrypt pp' bs) === Nothing
