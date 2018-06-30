
-- | Utilities for encrypting/decrypting byte arrays

module Dscp.Crypto.Encrypt
       ( -- * Passphrases
         PassPhrase
       , getPassPhrase
       , mkPassPhrase
       , minPassPhraseLength

         -- * Encrypted bytearray
       , Encrypted
       , eAuthTag
       , eCiphertext

         -- * Utility functions
       , encrypt
       , decrypt
       ) where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (AEAD, AEADMode (AEAD_GCM), AuthTag, BlockCipher (aeadInit),
                            Cipher (cipherInit), IV, aeadSimpleDecrypt, aeadSimpleEncrypt, nullIV)
import Crypto.Error (onCryptoFailure)
import Data.ByteArray (ByteArray, ByteArrayAccess)
import qualified Data.ByteArray as BA
import Fmt ((+|), (|+))

import Dscp.Crypto.Impl (hash)

-------------------------------------------------------------
-- Passphrases
-------------------------------------------------------------

newtype PassPhrase = PassPhrase
    { getPassPhrase :: ByteString
    } deriving (Eq, Ord, Show, Monoid, ByteArray, ByteArrayAccess)

-- | Minimal passphrase length. Should be enough.
minPassPhraseLength :: Int
minPassPhraseLength = 8

mkPassPhrase :: ByteString -> Either Text PassPhrase
mkPassPhrase bs
    | length bs < minPassPhraseLength = Left shortPassErr
    | otherwise = Right $ PassPhrase bs
  where
    shortPassErr = "Passphrase is too short, minimal length is "+|minPassPhraseLength|+" chars."

-------------------------------------------------------------
-- Encryption/decryption
-------------------------------------------------------------

-- | Datatype which combines ciphertext with cipher authentication tag.
data Encrypted ba = Encrypted
    { eAuthTag    :: !AuthTag
    , eCiphertext :: !ba
    } deriving (Eq)

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
            error $ "encrypt: impossible: " <> show err
        ppHashKey :: ByteString =
            BA.convert $ hash pp
        cipher :: CipherType =
            onCryptoFailure impossible identity $
            cipherInit ppHashKey
    in onCryptoFailure impossible identity $
       aeadInit aeadMode cipher initIV

-- | Encrypt given 'ByteArray' with AES.
encrypt :: ByteArray ba => PassPhrase -> ba -> Encrypted ba
encrypt pp plaintext =
    let aead = prepareAEAD pp
    in uncurry Encrypted $
       aeadSimpleEncrypt aead authHeader plaintext authTagLength

-- | Decrypt given 'Encrypted' datatype or fail, if passphrase
-- doesn't match.
decrypt :: ByteArray ba => PassPhrase -> Encrypted ba -> Maybe ba
decrypt pp (Encrypted tag ciphertext) =
    let aead = prepareAEAD pp
    in aeadSimpleDecrypt aead authHeader ciphertext tag
