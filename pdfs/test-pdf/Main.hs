
import qualified Data.ByteString as BS

import Pdf.FromLatex (ResourcePath (..), testData, produce)

main :: IO ()
main = do
    thing <- produce "russian" testData (ResourcePath "pdfs/template")
    putStrLn ("done" :: Text)
    BS.writeFile "result.pdf" thing
