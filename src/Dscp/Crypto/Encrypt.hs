{-# LANGUAGE DeriveFunctor #-}

-- | Utilities for encrypting/decrypting byte arrays

module Dscp.Crypto.Encrypt
       ( -- * Passphrases
         PassPhrase
       , PassPhraseError (..)
       , getPassPhrase
       , mkPassPhrase
       , minPassPhraseLength
       , maxPassPhraseLength

         -- * Encrypted bytearray
       , Encrypted (..)

         -- * Utility functions
       , DecryptionError (..)
       , encrypt
       , decrypt
       ) where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (AEAD, AEADMode (AEAD_GCM), AuthTag (..), BlockCipher (aeadInit),
                            Cipher (cipherInit), IV, aeadSimpleDecrypt, aeadSimpleEncrypt, nullIV)
import Crypto.Error (onCryptoFailure)
import Data.ByteArray (ByteArray, ByteArrayAccess)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.Text.Buildable (build)
import Fmt ((+|), (|+))
import Test.QuickCheck (Arbitrary (..), suchThatMap, vector)
import Test.QuickCheck.Instances ()
import Text.Show (show)

import Dscp.Crypto.ByteArray (FromByteArray (..))
import Dscp.Crypto.Impl (hash)
import Dscp.Util (leftToPanicWith)

-------------------------------------------------------------
-- Passphrases
-------------------------------------------------------------

-- | Newtype for passphrase. Uses blind instances for
-- 'Buildable' and 'Show' in order to avoid accidental
-- appearance of actual passphrases in logs.
newtype PassPhrase = PassPhrase
    { getPassPhrase :: ByteString
    } deriving (Eq, Ord, Monoid, ByteArray, ByteArrayAccess)

instance Buildable PassPhrase where
    build _ = "<passphrase>"

instance Show PassPhrase where
    show _ = "<passphrase>"

-- | Minimum passphrase length. Should be enough.
minPassPhraseLength :: Int
minPassPhraseLength = 8

-- | Maximum passphrase length. To keep it sane.
maxPassPhraseLength :: Int
maxPassPhraseLength = 128

-- | Errors which might happen during passphrase construction.
data PassPhraseError
    = PassPhraseTooShort
        { peLength :: !Int }
    | PassPhraseTooLong
        { peLength :: !Int }
    deriving (Eq)

instance Buildable PassPhraseError where
    build PassPhraseTooShort {..} =
        "Passphrase is too short ("+|peLength|+
        " chars), minimum length is "+|minPassPhraseLength|+" chars."
    build PassPhraseTooLong {..} =
        "Passphrase is too long ("+|peLength|+
        " chars), maximum length is "+|maxPassPhraseLength|+" chars."

instance Show PassPhraseError where
    show = toString . pretty

instance Exception PassPhraseError

-- | Smart constructor for a passphrase
mkPassPhrase :: ByteString -> Either PassPhraseError PassPhrase
mkPassPhrase bs
    | lbs < minPassPhraseLength = Left $ PassPhraseTooShort lbs
    | lbs > maxPassPhraseLength = Left $ PassPhraseTooLong lbs
    | otherwise = Right $ PassPhrase bs
  where
    lbs = length bs

instance Arbitrary PassPhrase where
    arbitrary = arbitrary `suchThatMap` (rightToMaybe . mkPassPhrase)

-------------------------------------------------------------
-- Encryption/decryption
-------------------------------------------------------------

-- | Datatype which combines ciphertext with cipher authentication tag.
data Encrypted ba = Encrypted
    { eAuthTag    :: !AuthTag
    , eCiphertext :: !ba
    } deriving (Eq, Functor)

instance Buildable ba => Show (Encrypted ba) where
    show = toString . pretty

instance Buildable ba => Buildable (Encrypted ba) where
    build Encrypted{..} = "encrypted "+|eCiphertext|+""

-- | Authentication tag length. Number 16 is the same as in the
-- `Crypto.MAC.Poly1305.Auth` tag datatype.
-- TODO: probably it can be made shorter without any problems?
authTagLength :: Int
authTagLength = 16

-- | Chosen cipher. We'll stick with AES256 for now.
-- TODO: are there better alternatives?
type CipherType = AES256

-- | Authentication header used for our encryption (empty one).
authHeader :: ByteString
authHeader = ""

-- | AEAD mode used for our encryption scheme.
-- We use GCM mode, because it's the most modern and performant one
-- among available in Cryptonite, and it's also unencumbered
-- by patents.
--
-- See: AEAD_OCB: https://en.wikipedia.org/wiki/OCB_mode
--      AEAD_CCM: https://en.wikipedia.org/wiki/CCM_mode
--      AEAD_CWC: https://en.wikipedia.org/wiki/CWC_mode
--      AEAD_EAX: https://en.wikipedia.org/wiki/EAX_mode
--      AEAD_GCM: https://en.wikipedia.org/wiki/Galois/Counter_Mode
aeadMode :: AEADMode
aeadMode = AEAD_GCM

-- | Initial vector for chosen cipher.
initIV :: IV CipherType
initIV = nullIV

-- | Prepare an AEAD context from a 'PassPhrase'.
prepareAEAD :: PassPhrase -> AEAD CipherType
prepareAEAD (PassPhrase pp) =
    let impossible err =
            error $ "encrypt: impossible: " <> Prelude.show err
        ppHashKey :: ByteString =
            BA.convert $ hash pp
        cipher :: CipherType =
            onCryptoFailure impossible identity $
            cipherInit ppHashKey
    in onCryptoFailure impossible identity $
       aeadInit aeadMode cipher initIV

-- | Encrypt given 'ByteArray' with AES.
encrypt :: FromByteArray ba => PassPhrase -> ba -> Encrypted ba
encrypt pp plaintext =
    let aead = prepareAEAD pp
        -- TODO: Subject to optimization. For now using converion to bytestring
        -- since @ba@ is not instance of 'ByteArray'.
        plaintextBS = BA.convert plaintext
        (authtag, ciphertextBS :: ByteString) =
            aeadSimpleEncrypt aead authHeader plaintextBS authTagLength
        ciphertext =
            leftToPanicWith "encrypt: got malformed item: " $
            fromByteArray ciphertextBS
    in  Encrypted authtag ciphertext

-- | Decrypt given 'Encrypted' datatype or fail, if passphrase
-- doesn't match.
decrypt :: FromByteArray ba => PassPhrase -> Encrypted ba -> Either DecryptionError ba
decrypt pp (Encrypted tag ciphertext) = do
    let aead = prepareAEAD pp
        ciphertextBS = BA.convert ciphertext :: ByteString
    plaintextBS <-
          maybeToRight PassPhraseInvalid $
          aeadSimpleDecrypt aead authHeader ciphertextBS tag
    return $
        leftToPanicWith "decrypt: got malformed item: " $
        fromByteArray plaintextBS

instance Arbitrary a => Arbitrary (Encrypted a) where
    arbitrary =
        Encrypted
        <$> (AuthTag . BA.convert . BS.pack <$> vector authTagLength)
        <*> arbitrary

-- | Errors which might occur during decryption
data DecryptionError = PassPhraseInvalid
    deriving (Eq)

instance Buildable DecryptionError where
    build PassPhraseInvalid = "Invalid passphrase is used for decryption"

instance Show DecryptionError where
    show = toString . pretty

instance Exception DecryptionError
