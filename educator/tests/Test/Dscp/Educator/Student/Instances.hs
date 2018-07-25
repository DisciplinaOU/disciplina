module Test.Dscp.Educator.Student.Instances () where

import Dscp.Util.Test

import Dscp.Educator.Web.Student (IsFinal (..))

deriving instance Arbitrary IsFinal
