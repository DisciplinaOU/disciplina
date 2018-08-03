module Test.Dscp.Educator.Web.Instances () where

import Dscp.Util.Test

import Dscp.Educator.Web.Types (IsFinal (..))

deriving instance Arbitrary IsFinal
