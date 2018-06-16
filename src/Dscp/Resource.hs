-- Can't use autoexporter because of dodgy instances. TODO figure it out.
-- {-# OPTIONS_GHC -F -pgmF autoexporter #-}
module Dscp.Resource (module M) where

import Dscp.Resource.Class as M
import Dscp.Resource.Logging as M
import Dscp.Resource.Other ()
