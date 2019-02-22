module Test.Dscp.Educator.Web.Educator.Swagger where

import Test.QuickCheck (once, total)

import Dscp.Educator.Web.Educator
import Dscp.Util.Test
import Dscp.Web.Swagger

spec_Educator_API_swagger :: Spec
spec_Educator_API_swagger = do
    it "Builds without errors" . once $ do
        total $ encodeSwagger educatorAPISwagger
