-- | Some missing instances in library.

module Dscp.Crypto.Instances () where

import Crypto.Cipher.Types (AuthTag (..))
import Data.ByteArray (ByteArray)

deriving instance Ord AuthTag
deriving instance Monoid AuthTag
-- | Need this for educator secret key deserialisation.
deriving instance ByteArray AuthTag
