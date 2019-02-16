
module Pdf.Scanner (inject, project, insertionMark, PDFBody (..), MaxSearchLength (..)) where

import Data.ByteString.Char8  as LBS
import Data.ByteString.Base64 as LBS
import Data.Hashable

newtype PDFBody = PDFBody { getPDFBody :: LBS.ByteString }

newtype MaxSearchLength = MaxSearchLength { getMaxSearchLength :: Maybe Int }

inject :: MaxSearchLength -> LBS.ByteString -> PDFBody -> Maybe PDFBody
inject
    (fromMaybe maxBound . getMaxSearchLength -> quota)
     thing
    (PDFBody text)
  =
    let base64    = LBS.encode thing
        commented = "\n%" <> fairCVStartMark <> "{" <> base64 <> "}%"
    in  do
        place <- findFromEnd quota insertionMark text
        return $ PDFBody $ insertAt place commented text
  where

project :: MaxSearchLength -> PDFBody -> Maybe LBS.ByteString
project
    (fromMaybe maxBound . getMaxSearchLength -> quota)
    (PDFBody text)
  = do
    place <- findFromEnd quota fairCVStartMark text
    let after = LBS.drop (place + LBS.length fairCVStartMark + 1) text
        piece = LBS.takeWhile (not . (== '}')) after

    either (const Nothing) pure
        $ LBS.decode piece

findFromEnd :: Int -> LBS.ByteString -> LBS.ByteString -> Maybe Int
findFromEnd quota what text = do
    guard $ not $ LBS.null what
    guard $ not $ LBS.null text
    go quota (LBS.length text - LBS.length what)
  where
    go 0 _         = Nothing
    go _ i | i < 0 = Nothing
    go q i = do
        if LBS.index text i == LBS.head what
            && equalsFrom i what text
        then Just i
        else go (q - 1) (i - 1)

equalsFrom :: Int -> LBS.ByteString -> LBS.ByteString -> Bool
equalsFrom offset what text = do
    what == LBS.take (LBS.length what) (LBS.drop offset text)

insertAt :: Int -> LBS.ByteString -> LBS.ByteString -> LBS.ByteString
insertAt place what text =
    let (before, after) = LBS.splitAt place text
    in   before <> what <> after

insertionMark :: LBS.ByteString
insertionMark = "\nstartxref"

fairCVStartMark :: LBS.ByteString
fairCVStartMark = text <> ":" <> show (hash text)
  where
    text = "fairCV-start-mark"
