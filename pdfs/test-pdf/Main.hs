
import qualified Data.ByteString as BS

import Pdf.FromLatex (Language (..), ResourcePath (..), produce, testData)

main :: IO ()
main = do
    thing <- produce RU "Абыр Валг" testData (ResourcePath "pdfs/template")
    BS.writeFile "result.pdf" thing
