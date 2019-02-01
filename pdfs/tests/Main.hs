
import Test.QuickCheck (property, Arbitrary (..))
import Data.ByteString.Char8 as LBS

import Pdf.Scanner

import Test.Hspec (hspec, describe, it)

instance Arbitrary LBS.ByteString where
    arbitrary = LBS.pack `fmap` arbitrary

main :: IO ()
main = hspec $ do
    let inf  = MaxSearchLength Nothing
        zero = MaxSearchLength (Just 0)

    describe "injection/projection works" $ do
        it "project _ x (inject _ x text) == x" . property $
            \(before, after, piece) ->
                let
                    text = before <> insertionMark <> after
                in
                    (project inf =<< inject inf piece (PDFBody text))
                        == Just piece

        it "inject fails if no insert mark present" . property $
            \(text, piece) ->
                getPDFBody `fmap` inject inf piece (PDFBody text)
                    == Nothing

        it "inject fails if quota is exceeded" . property $
            \(before, after, piece) ->
                let
                    text = before <> insertionMark <> after
                in
                    getPDFBody `fmap` inject zero piece (PDFBody text)
                        == Nothing

        it "project fails if quota is exceeded" . property $
            \(before, after, piece) ->
                let
                    text = before <> insertionMark <> after
                in
                    (project zero =<< inject inf piece (PDFBody text))
                        == Nothing

        it "project fails if nothing was inserted" . property $
            \(text) ->
                project inf (PDFBody text)
                    == Nothing
