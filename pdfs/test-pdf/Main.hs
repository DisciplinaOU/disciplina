
import qualified Data.ByteString.Lazy as LBS

import Pdf.FromLatex (CertificateIssuerInfo (..), Language (..), LatexPath (..), ResourcePath (..),
                      produce, testData)
import Pdf.Scanner (PDFBody (..))

main :: IO ()
main = do
    thing <- produce RU (CertificateIssuerInfo "Абыр Валг" "http://example.com")
             testData (LatexPath "xelatex") (ResourcePath "pdfs/template")
    LBS.writeFile "result.pdf" (getPDFBody thing)
