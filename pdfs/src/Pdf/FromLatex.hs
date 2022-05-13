{- |
    This module contains tools to produce PDF from data and locale using Latex.
-}

module Pdf.FromLatex
    ( -- * Re-exported from `disciplina-core`
      CertificateFullInfo
    , Language (..)

      -- * Info about the Educator
    , CertificateIssuerInfo (..)
      -- * Path to LaTeX generator implementation (i. e. `xelatex`)
    , LatexPath (..)
      -- * Path to resources for latex generation
    , ResourcePath(..)
      -- * Base url for download link in QR code
    , DownloadBaseUrl (..)

      -- * PDF generator
    , produce

      -- * Self-test data
    , testData
    )
    where

import Universum
import Codec.Picture (DynamicImage (..), savePngImage)
import Codec.QRCode (ErrorLevel (..), QRImage (..), TextEncoding (..), defaultQRCodeOptions, encode)
import Codec.QRCode.JuicyPixels (toImage)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.Builder as Text
import Data.Time.Calendar
import GHC.Exts (fromList)
import qualified System.Directory as D
import System.FilePath.Posix ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (byteStringInput, byteStringOutput, proc, runProcess, setStdin,
                             setStdout, setWorkingDir)
import Text.Printer (text)

import Dscp.Core.Foundation
import Dscp.Core.Web
import Dscp.Util

import Pdf.MkLatex
import Pdf.Scanner

-- | Generate latex certificate from locale, Educator name and data.
generate :: Language -> CertificateIssuerInfo -> CertificateFullInfo -> Text
generate lang ciInfo cert =
    Text.toStrict $ Text.toLazyText $ make lang (ciInfo, cert)
  where
    MkLatex make = fullInfo

-- | Converter for certificate data into latex.
fullInfo :: MkLatex (CertificateIssuerInfo, CertificateFullInfo)
fullInfo
    = split (const ())      language
    $ split (cfiMeta . snd) personal
    $ inBlock "document"
        $ divide (id &&& (toList . cfiGrades . snd))
            meta
            courses
  where
    language = localized $ \case
        RU -> command "documentclass[11pt, russian]" $ const [text "faircv"]
        EN -> command "documentclass[11pt, english]" $ const [text "faircv"]
        ES -> command "documentclass[11pt, spanish]" $ const [text "faircv"]
        ZH -> command "documentclass[11pt, chinese]" $ const [text "faircv"]

    personal
        = split cmStudentName               (command "Name"        $ pure . shownDesc)
        $ split cmStudentBirthDate          (command "DateOfBirth" $ formatDate)
        $ split (const ())                  (command "QR"          $ const ["qrcode"])
        $ ignore

    meta
        = split (const ())                  (command "MakeHeader"  $ const [])
        $ split (ciiName . fst)             (command "section"     $ pure . shownDesc)
        $ split (ciiWebsite . fst)          (command "EducatorUrl" $ pure . shownDesc)
        $ split (cfiMeta . snd)             (inBlock "Diploma"       diploma)
        $ ignore

    diploma
        = split (cmStartYear &&& cmEndYear) (command "EducationPeriod" $ showBoth)
        $ split  cmNumber                   (command "DiplomaId"       $ pure . shown)
        $ split  cmIssueDate                (command "DateOfIssue"     $ formatDate)
        $ split  cmTitle                    (command "DegreeLevel"     $ pure . shownDesc)
        $ split  cmMajor                    (command "Major"           $ pure . shownDesc)
        $ split  cmSpecialization           (command "Minor"           $ pure . maybe "---" shownDesc)
        $ split  cmEducationForm             educationForm
        $ ignore

    educationForm = choose (\case Parttime -> Left (); _ -> Right ())
        (command "PartTimeEducation" (const []))
        ignore

    courses =
        inBlock "Courses"
            $ allThe (the "\\\\") course
      where
        course
            = custom
            $ \lang CertificateGrade { cgSubject, cgLang, cgHours, cgScale, cgGrade = UnsafeGrade grade} -> ""
                <> shownDesc cgSubject <> " & "
                <> shown     cgLang    <> " & "
                <> shown     cgHours   <> " & "
                -- <> maybe "---" shown cgCredits <> " & "
                <> renderGrade lang cgScale grade

        renderGrade _ RusDiff grade
            | grade >= 100 = "5"
            | grade >= 80 = "4"
            | grade >= 60 = "3"
            | grade >= 40 = "2"
            | otherwise = "1"
        renderGrade RU RusNonDiff grade
            | grade >= 100 = "зачёт"
            | otherwise = "незачёт"
        renderGrade EN RusNonDiff grade
            | grade >= 100 = "passed"
            | otherwise = "not passed"
        renderGrade ES RusNonDiff grade
            | grade >= 100 = "aprobado"
            | otherwise = "no aprobado"
        renderGrade ZH RusNonDiff grade
            | grade >= 100 = "通过"
            | otherwise = "未通过"


    formatDate day = [shown d, shown m, shown y]
      where
        (y, m, d) = toGregorian day

    showBoth (a, b) = [shown a, shown b]
    shownDesc       = text . escapeInLatex . unItemDesc

-- | Type wrapper for latex executable path
newtype LatexPath = LatexPath
    { unLatexPath :: FilePath
    } deriving (Show, Eq)

-- | Type wrapper for latex resource path.
newtype ResourcePath = ResourcePath
    { unResourcePath :: FilePath
    } deriving (Show, Eq)

-- | Type wrapper for download base url for QR code link.
newtype DownloadBaseUrl = DownloadBaseUrl
    { unDownloadBaseUrl :: BaseUrl
    } deriving (Show, Eq)

-- | Generate a PDF-certificate and return it as a bytestring.
produce ::
       MonadIO m
    => Language
    -> CertificateIssuerInfo
    -> CertificateFullInfo
    -> LatexPath
    -> ResourcePath
    -> DownloadBaseUrl
    -> m PDFBody
produce loc ciInfo info
    (LatexPath xelatex)
    (ResourcePath resPath)
    (DownloadBaseUrl url) = liftIO $

    -- Everything produced should be removed.
    -- This may lead to /tmp exhaustion attack, unless /tmp or memory
    -- is big enough.
    withSystemTempDirectory "faircv" $ \tmpPath -> do
        -- Latex reads and writes in the same dir - lets isolate it.
        copyDirectory resPath tmpPath

        makeQr ciInfo info url (tmpPath </> "qrcode.png")

        let theText = generate loc ciInfo info
        let input   = encodeUtf8 theText

        let action
                = runProcess
                $ setWorkingDir tmpPath
                $ setStdin (byteStringInput input)  -- feed the latex doc in directly
                $ setStdout byteStringOutput
                $ proc xelatex []

        -- LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right.
        --
        -- That's why. And there's no picture behind header if only 1 action is run.
        _ <- action
        _ <- action

        pdf <- LBS.readFile (tmpPath </> "texput.pdf")
        evaluateNF $ PDFBody pdf

makeQr
    :: MonadIO m
    => CertificateIssuerInfo
    -> CertificateFullInfo
    -> BaseUrl
    -> FilePath
    -> m ()
makeQr ciInfo info baseUrl path =
    let eAddr = ciiId ciInfo
        cId = getId $ cfiMeta info
        cName = toUrlPiece $ CertificateName eAddr cId
        prefix = toText $ showBaseUrl baseUrl

        qrCode = nothingToPanic "Came up with too long URL" $
            encode (defaultQRCodeOptions L) Iso8859_1 $
            prefix <> "/" <> cName
        resultImgSize = 328 -- size of original image in PDF
        scale = resultImgSize `div` qrImageSize qrCode
        image = toImage 4 scale qrCode

    in liftIO $ savePngImage path (ImageY8 image)

copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory from to = do
    isDir <- D.doesDirectoryExist from
    if isDir
    then do
        D.createDirectoryIfMissing True to
        subs <- D.listDirectory from
        forM_ subs $ \sub ->
             copyDirectory (from </> sub) (to </> sub)
    else
        D.copyFile from to

-- | Data for self-test.
testData :: CertificateFullInfo
testData = CertificateFullInfo
    { cfiMeta = CertificateMeta
        { cmSpecialization   = Just "\"Владение ~ черпаком & ведром\""
        , cmMajor            = "123 Черпание\\dropTable{\"students\"}"
        , cmTitle            = "Младший\nпомошник\rстаршего\tчерпальщика\\ \\\\ "
        , cmNumber           = 100500
        , cmEducationForm    = Parttime
        , cmIssueDate        = fromGregorian 2015 5 13
        , cmStartYear        = 2010
        , cmEndYear          = 2015
        , cmStudentName      = "Вася Пупкин"
        , cmStudentBirthDate = fromGregorian 1990 2 3
        }
    , cfiGrades = fromList
        [ CertificateGrade
            { cgGrade = minBound
            , cgCredits = Just 132
            , cgHours = 123
            , cgLang = RU
            , cgScale = RusNonDiff
            , cgSubject = "Следование за обозом"
            }
        , CertificateGrade
            { cgGrade = maxBound
            , cgCredits = Nothing
            , cgHours = 13
            , cgLang = RU
            , cgScale = RusDiff
            , cgSubject = "Черпание"
            }
        , CertificateGrade
            { cgGrade = minBound
            , cgCredits = Just 34
            , cgHours = 1
            , cgLang = EN
            , cgScale = RusNonDiff
            , cgSubject = "Сопротивление холере"
            }
        ]
    }
