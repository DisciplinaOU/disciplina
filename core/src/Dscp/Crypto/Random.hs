-- | Secure generation of random numbers and bytestrings.

module Dscp.Crypto.Random
       ( SecureRandomM(..)
       , runSecureRandom
       , secureRandomBS

       , randomNumber
       , randomNumberInRange
       ) where

import Crypto.Number.Basic (numBytes)
import Crypto.Number.Serialize (os2ip)
import Crypto.OpenSSL.Random (randBytes)
import Crypto.Random.Types (MonadRandom, getRandomBytes)
import qualified Data.ByteArray as ByteArray (convert)

-- | Generate a cryptographically random 'ByteString' of specific length.
secureRandomBS :: MonadIO m => Int -> m ByteString
secureRandomBS = liftIO . randBytes

-- | You can use 'runSecureRandom' on any 'MonadRandom' computation to make
-- it use a Really Secure™ randomness source (that is, OpenSSL).
newtype SecureRandomM a = SecureRandomM {runSecureRandomIO :: IO a}
    deriving (Functor, Applicative, Monad)

runSecureRandom :: MonadIO m => SecureRandomM a -> m a
runSecureRandom = liftIO . runSecureRandomIO

instance MonadRandom SecureRandomM where
    getRandomBytes n = SecureRandomM (ByteArray.convert <$> secureRandomBS n)

-- | Generate a random number in range [0, n).
--
-- We want to avoid modulo bias, so we use the arc4random_uniform
-- implementation (http://stackoverflow.com/a/20051580/615030). Specifically,
-- we repeatedly generate a random number in range [0, 2^x) until we hit on
-- something outside of [0, 2^x mod n), which means that it'll be in range
-- [2^x mod n, 2^x). The amount of numbers in this interval is guaranteed to
-- be divisible by n, and thus applying 'mod' to it will be safe.
randomNumber :: MonadRandom m => Integer -> m Integer
randomNumber n
    | n <= 0 = error "randomNumber: n <= 0"
    | otherwise = gen
  where
    size = max 4 (numBytes n)             -- size of integers, in bytes
    rangeMod = 2 ^ (size * 8) `rem` n     -- 2^x mod n
    gen = do
        x <- os2ip @ByteString <$> getRandomBytes size
        if x < rangeMod then gen else return (x `rem` n)

-- | Generate a random number in range [a, b].
randomNumberInRange :: MonadRandom m => Integer -> Integer -> m Integer
randomNumberInRange a b
    | a > b     = error "randomNumberInRange: a > b"
    | otherwise = (a +) <$> randomNumber (b - a + 1)
