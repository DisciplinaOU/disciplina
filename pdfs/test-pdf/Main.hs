
import Universum
import qualified Data.ByteString.Lazy as LBS

import Dscp.Core.Web (parseBaseUrl)

import Pdf.FromLatex (CertificateIssuerInfo (..), DownloadBaseUrl (..), Language (..),
                      LatexPath (..), ResourcePath (..), produce, testData)
import Pdf.Scanner (PDFBody (..))

main :: IO ()
main = do
    baseUrl <- parseBaseUrl "https://educator.disciplina.io/api/certificates/v1/cert"
    thing <- produce RU
             (CertificateIssuerInfo "Абыр Валг" "http://example.com" "Principal")
             testData
             (LatexPath "xelatex")
             (ResourcePath "pdfs/template")
             (DownloadBaseUrl baseUrl)
    LBS.writeFile "result.pdf" (getPDFBody thing)
