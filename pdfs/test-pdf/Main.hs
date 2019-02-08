
import qualified Data.ByteString.Lazy as LBS

import Pdf.FromLatex (Language (..), ResourcePath (..), produce, testData)

main :: IO ()
main = do
    thing <- produce RU "Абыр Валг" testData (ResourcePath "pdfs/template")
    LBS.writeFile "result.pdf" thing
