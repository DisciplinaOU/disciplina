
import qualified Data.ByteString.Lazy as LBS

import Pdf.FromLatex (EducatorInfo (..), Language (..), ResourcePath (..), produce, testData)

main :: IO ()
main = do
    thing <- produce RU (EducatorInfo "Абыр Валг" "http://example.com") testData (ResourcePath "pdfs/template")
    LBS.writeFile "result.pdf" thing
