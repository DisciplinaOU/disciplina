
import qualified Data.ByteString as BS

import Pdf.Scanner

main :: IO ()
main = do
    getArgs >>= \case
        [pdf, json, out] -> do
            pdfText  <- LBS.readFile pdf
            jsonText <- LBS.readFile json

            LBS.length pdfText `seq`
                case inject (MaxSearchLength (Just 2048)) jsonText (PDFBody pdfText) of
                    Nothing -> do
                        error . Text.pack $ "Cannot inject <" <> json <> "> into <" <> pdf <> ">."

                    Just (PDFBody it) -> do
                        LBS.writeFile out it
        _ -> do
            error "USAGE: dscp-inject-json-into-pdf <pdf-in> <json> <pdf-out>"
