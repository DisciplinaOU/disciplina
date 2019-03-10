
module Pdf.Scanner
    ( inject
    , project
    , exclude
    , unInject
    , insertionMark
    , PDFBody(..)
    , writePdf
    , MaxSearchLength(..)
    ) where

import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import Data.Char (ord)
import Data.Hashable
import Fmt (Buildable (..))

newtype PDFBody = PDFBody { getPDFBody :: LBS.ByteString }
    deriving (Eq, Show, NFData, Generic)

instance Buildable PDFBody where
    build _ = "<pdf>"

writePdf :: MonadIO m => FilePath -> PDFBody -> m ()
writePdf path (PDFBody pdf) = liftIO $ LBS.writeFile path pdf

newtype MaxSearchLength = MaxSearchLength { getMaxSearchLength :: Maybe Int }

inject :: MaxSearchLength -> ByteString -> PDFBody -> Maybe PDFBody
inject
    (fromMaybe maxBound . getMaxSearchLength -> quota)
     thing
    (PDFBody text)
  =
    let base64    = LBS.fromStrict $ Base64.encode thing
        commented = "\n%" <> fairCVStartMark <> "{" <> base64 <> "}%"
    in  do
        place <- findFromEnd quota insertionMark text
        return $ PDFBody $ insertAt place commented text

exclude :: MaxSearchLength -> PDFBody -> Maybe PDFBody
exclude msl body = snd <$> unInject msl body

project :: MaxSearchLength -> PDFBody -> Maybe ByteString
project msl body = fst <$> unInject msl body

unInject :: MaxSearchLength -> PDFBody -> Maybe (ByteString, PDFBody)
unInject
    (fromMaybe maxBound . getMaxSearchLength -> quota)
    (PDFBody text)
  = do
    place <- findFromEnd quota fairCVStartMark text
    let after  = LBS.drop (place + LBS.length fairCVStartMark + 1) text
        mark   = fromIntegral $ ord '}'
        piece  = LBS.takeWhile (/= mark) after
        source = LBS.take (place - 2) text <> LBS.drop (place + LBS.length piece + LBS.length fairCVStartMark + 1 + 2) text

    either
        (const Nothing)
        (\json -> pure (json, PDFBody source))
        $ Base64.decode (LBS.toStrict piece)

findFromEnd :: Int -> LBS.ByteString -> LBS.ByteString -> Maybe Int64
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

equalsFrom :: Int64 -> LBS.ByteString -> LBS.ByteString -> Bool
equalsFrom offset what text = do
    what == LBS.take (LBS.length what) (LBS.drop offset text)

insertAt :: Int64 -> LBS.ByteString -> LBS.ByteString -> LBS.ByteString
insertAt place what text =
    let (before, after) = LBS.splitAt place text
    in   before <> what <> after

insertionMark :: LBS.ByteString
insertionMark = "\nstartxref"

fairCVStartMark :: LBS.ByteString
fairCVStartMark = text <> ":" <> show (hash text)
  where
    text = "fairCV-start-mark"
