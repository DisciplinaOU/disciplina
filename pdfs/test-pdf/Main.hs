
import qualified Data.ByteString as BS

import Pdf.FromLatex (ResourcePath (..), Language (..), testData, produce)

main :: IO ()
main = do
    thing <- produce RU testData (ResourcePath "pdfs/template")
    BS.writeFile "result.pdf" thing
