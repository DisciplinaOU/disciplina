
module Dscp.Snowdrop.Util
       ( (==>)
       ) where

import Snowdrop.Util

--------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------

(==>)
    :: forall id value k v
    .  HasReview k id
    => HasReview v value
    => id
    -> value
    -> (k, v)
k ==> v = (inj k, inj v)
