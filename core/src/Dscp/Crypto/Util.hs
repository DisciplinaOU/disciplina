-- | Small utility functions

module Dscp.Crypto.Util
       ( constTimeEq
       ) where

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
