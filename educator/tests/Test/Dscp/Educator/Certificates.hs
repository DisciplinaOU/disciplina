{-# LANGUAGE OverloadedLists #-}

module Test.Dscp.Educator.Certificates where

import Control.Lens (ix)
import Data.Bits (complement)
import qualified Data.ByteString.Lazy as LBS
import Data.Default (def)
import qualified Pdf.FromLatex as Pdf
import qualified Pdf.Scanner as Pdf
import Servant.Util (asc, fullContent)

import Dscp.DB.SQL
import Dscp.Educator
import Dscp.Educator.Web.Educator
import Dscp.Util.Test
import Dscp.Witness.Web

import Test.Dscp.DB.SQL.Mode
import Test.Dscp.Educator.Mode

spec_Educator_certificates :: Spec
spec_Educator_certificates = specWithTempPostgresServer $ do
    -- each PDF production takes 2 sec, so running less tests
    divideMaxSuccessBy 10 $ do
        it "Can build a full student certificate flawlessly" $ \_ -> property $
            \lang issuer cert faircv -> ioProperty $ do
                rawPdf <- Pdf.produce lang issuer cert
                          testLatexPath testResourcePath testDownloadBaseUrl
                pdf <- embedFairCVToCert faircv rawPdf
                return $ total pdf

        describe "Certificate endpoints" $ do
            it "Can add a certificate" $ educatorPropertyM $ do
                cert <- pickSmall arbitrary
                void $ lift $ educatorAddCertificate cert

            it "Added certificate is fetchable and verifiable" $ educatorPropertyM $ do
                cert <- pickSmall arbitrary

                pdf <- lift $ do
                    void $ educatorAddCertificate cert

                    [crt] <- invoke $ educatorGetCertificates def def
                    invoke $ educatorGetCertificate $ cId crt

                let pdfBs = Pdf.getPDFBody pdf
                    pdfLen = LBS.length pdfBs

                flippedByteIdx <- pickSmall $ choose (0, pdfLen - 1)
                let badPdf = Pdf.PDFBody $
                        pdfBs & ix flippedByteIdx %~ complement

                lift $ do
                    void updateMempoolWithPublications
                    checkRes <- checkFairCVPDF pdf
                    checkResBad <- do
                        let handler e = case (e :: WitnessAPIError) of
                                InvalidFormat -> return False
                                _             -> throwM e

                        (fairCVFullyValid . fcacrCheckResult <$> checkFairCVPDF badPdf)
                            `catch` handler

                    let positive = counterexample "FairCV is not verified" $
                                   fairCVFullyValid $ fcacrCheckResult checkRes
                        negative = counterexample "Bad FairCV is verified" $
                                   not checkResBad
                    return $ positive .&&. negative

            it "Sorting certificates on creation day works" $ educatorPropertyM $ do
                n <- pick $ choose (0, 5)
                certs <- pickSmall $ replicateM n arbitrary

                lift $ do
                    forM_ certs educatorAddCertificate
                    certs' <- invoke $ educatorGetCertificates [asc #createdAt] fullContent

                    return $ map cMeta certs'
                             ===
                             sortOn cmIssueDate (map cfiMeta certs)
