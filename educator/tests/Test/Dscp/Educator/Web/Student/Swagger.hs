module Test.Dscp.Educator.Web.Student.Swagger where

import Test.QuickCheck (once, total)

import Dscp.Educator.Web.Student
import Dscp.Util.Test
import Dscp.Web.Swagger

spec_Student_API_swagger :: Spec
spec_Student_API_swagger = do
    it "Builds without errors" . once $ \addr -> do
        total $ encodeSwagger (studentAPISwagger addr)
