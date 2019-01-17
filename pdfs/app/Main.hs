
import Data.ByteString as LBS
import Data.Text as Text

import Pdf.Scanner

main :: IO ()
main = do
    [pdf, json, out] <- getArgs

    pdfText  <- LBS.readFile pdf
    jsonText <- LBS.readFile json

    LBS.length pdfText `seq`
        case inject (MaxSearchLength (Just 2048)) jsonText (PDFBody pdfText) of
            Nothing -> do
                error . Text.pack $ "Cannot inject <" <> json <> "> into <" <> pdf <> ">."

            Just (PDFBody it) -> do
                LBS.writeFile out it
