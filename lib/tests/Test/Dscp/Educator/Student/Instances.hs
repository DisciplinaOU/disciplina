module Test.Dscp.Educator.Student.Instances () where

import Test.Common

import Dscp.Educator.Web.Student (IsFinal (..))

deriving instance Arbitrary IsFinal
