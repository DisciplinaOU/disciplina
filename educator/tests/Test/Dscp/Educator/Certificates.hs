{-# LANGUAGE OverloadedLists #-}

module Test.Dscp.Educator.Certificates where

import Data.Default (def)
import qualified Pdf.FromLatex as Pdf
import Servant.Util (asc, fullContent)

import Dscp.DB.SQL
import Dscp.Educator
import Dscp.Educator.Web.Educator
import Dscp.Util.Test

import Test.Dscp.DB.SQL.Mode
import Test.Dscp.Educator.Mode

spec_Educator_certificates :: Spec
spec_Educator_certificates = specWithTempPostgresServer $ do
    -- each PDF production takes 2 sec, so running less tests
    divideMaxSuccessBy 10 $ do
        it "Can build a full student certificate flawlessly" $ \_ -> property $
            \lang issuer cert faircv -> ioProperty $ do
                rawPdf <- Pdf.produce lang issuer cert testLatexPath testResourcePath
                pdf <- embedFairCVToCert faircv rawPdf
                return $ total pdf

        describe "Certificate endpoints" $ do
            it "Can add a certificate" $ educatorPropertyM $ do
                cert <- pickSmall arbitrary
                lift $ educatorAddCertificate cert

            it "Added certificate is fetchable" $ educatorPropertyM $ do
                cert <- pickSmall arbitrary

                lift $ do
                    educatorAddCertificate cert

                    [cId -> certId] <- invoke $ educatorGetCertificates def def
                    pdf <- invoke $ educatorGetCertificate certId

                    return $ total pdf

                    -- TODO: I would be happy to verify the certificate in place,
                    -- but that's not possible yet, to be resolved in [DSCP-464].

                    -- checkRes <- checkFairCVPDF pdf
                    -- return $ counterexample "FairCV is not verified" $
                    --          fairCVFullyValid $ fcacrCheckResult checkRes

            it "Sorting certificates on creation day works" $ educatorPropertyM $ do
                n <- pick $ choose (0, 5)
                certs <- pickSmall $ replicateM n arbitrary

                lift $ do
                    forM_ certs educatorAddCertificate
                    certs' <- invoke $ educatorGetCertificates [asc #createdAt] fullContent

                    return $ map cMeta certs'
                             ===
                             sortOn cmIssueDate (map cfiMeta certs)