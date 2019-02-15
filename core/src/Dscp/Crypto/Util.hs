-- | Small utility functions

module Dscp.Crypto.Util
       ( constTimeEq
       , secretFromSeed
       ) where

import Dscp.Crypto.Impl (SecretKey, genSecretKey, withIntSeed, withSeed)

import qualified Crypto.Util as CU (constTimeEq)
import Data.ByteArray (ByteArrayAccess, convert)

-- | Checks two bytestrings for equality without breaches for
-- timing attacks.
--
-- Adapts 'Crypto.Util.constTimeEq' for any 'ByteArrayAccess' instance.
-- See <http://codahale.com/a-lesson-in-timing-attacks/> for more info.
constTimeEq
    :: (ByteArrayAccess b1, ByteArrayAccess b2)
    => b1 -> b2 -> Bool
constTimeEq b1 b2 = CU.constTimeEq (convert b1) (convert b2)

-- | This is how we generate secret keys from user-supplied seed
secretFromSeed :: ByteString -> SecretKey
secretFromSeed input =
    fromMaybe (withSeed input genSecretKey) secretFromIntSeed
  where
    secretFromIntSeed = do
        -- we need this clause as soon as many code uses
        -- 'withIntSeed' for generation, for instance list of students
        -- known to educator in Student API
        seed <- readMaybe @Word . toString @Text $ decodeUtf8 input
        return $ withIntSeed (fromIntegral seed) genSecretKey
