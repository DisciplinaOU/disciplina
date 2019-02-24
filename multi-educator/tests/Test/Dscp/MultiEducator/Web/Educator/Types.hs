module Test.Dscp.MultiEducator.Web.Educator.Types where

import Dscp.MultiEducator.Web.Educator.Types
import Dscp.Util.Test

spec_HttpDataSerialisation :: Spec
spec_HttpDataSerialisation = describe "MultiEducator types URL serialisation" $
    httpApiRoundtripProp @CertificateName
