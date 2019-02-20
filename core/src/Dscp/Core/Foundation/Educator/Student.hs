
module Dscp.Core.Foundation.Educator.Student where

import Dscp.Crypto
import Dscp.Util

-- | Student is identified by their public address.
type Student = Address

instance HasId Student
