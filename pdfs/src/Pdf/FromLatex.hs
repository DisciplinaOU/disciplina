
{- |
    This module contains tools to produce PDF from data and locale using Latex.
-}

module Pdf.FromLatex
    ( -- * Re-exported from `disciplina-core`
      CertificateFullInfo
    , Language (..)

      -- * Info about the Educator
    , CertificateIssuerInfo (..)
      -- * Path to resources for latex generation
    , ResourcePath(..)

      -- * PDF generator
    , produce

      -- * Self-test data
    , testData
    )
    where

import Control.Arrow ((&&&))
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.Builder as Text
import Data.Time.Calendar (fromGregorian, toGregorian)
import qualified System.Directory as D
import System.FilePath.Posix ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (byteStringInput, byteStringOutput, runProcess, setStdin, setStdout)
import Text.Printer (text)

import Dscp.Core.Foundation.Educator

import Pdf.MkLatex

-- | Datatype containing information about Educator which issued
-- the certificate, required in order to render a certificate.
data CertificateIssuerInfo = CertificateIssuerInfo
    { ciiName :: ItemDesc
    , ciiUrl  :: ItemDesc
    } deriving (Show, Eq, Generic)

-- | Generate latex certificate from locale, Educator name and data.
generate :: Language -> CertificateIssuerInfo -> CertificateFullInfo -> Text
generate lang ciInfo cert =
    Text.toStrict $ Text.toLazyText $ make (lang, (ciInfo, cert))
  where
    MkLatex make = fullInfo

-- | Converter for certificate data into latex.
fullInfo :: MkLatex (Language, (CertificateIssuerInfo, CertificateFullInfo))
fullInfo
    = divided               language
    $ split (cfiMeta . snd) personal
    $ inBlock "document"
        $ divide (id &&& (cfiGrades . snd))
            meta
            courses
  where
    language = choose (\case RU -> Left (); EN -> Right ())
        (command "documentclass[11pt, russian]" $ const [text "faircv"])
        (command "documentclass[11pt, english]" $ const [text "faircv"])

    personal
        = split cmStudentName               (command "Name"        $ pure . shownDesc)
        $ split cmStudentBirthDate          (command "DateOfBirth" $ formatDate)
        $ split (const ())                  (command "QR"          $ const ["images/example-qr"])
        $ ignore

    meta
        = split (const ())                  (command "MakeHeader"  $ const [])
        $ split (ciiName . fst)              (command "section"     $ pure . shownDesc)
        $ split (ciiUrl . fst)               (command "EducatorUrl" $ pure . shownDesc)
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
            $ allThe (custom $ const "\\\\")
                course
      where
        course
            = custom
            $ \CertificateGrade { cgSubject, cgLang, cgHours, cgCredits, cgGrade = UnsafeGrade grade} -> ""
                <> shownDesc cgSubject <> " & "
                <> shown     cgLang    <> " & "
                <> shown     cgHours   <> " & "
                <> maybe "---" shown cgCredits <> " & "
                <> shown     grade

    formatDate day = [shown d, shown m, shown y]
      where
        (y, m, d) = toGregorian day

    showBoth (a, b) = [shown a, shown b]
    shownDesc       = text . escapeInLatex . unItemDesc

-- | Type wrapper for latex resource path.
newtype ResourcePath = ResourcePath { unResourcePath :: FilePath }

-- | Generate a PDF-certificate and return it as a bytestring.
produce :: Language -> CertificateIssuerInfo -> CertificateFullInfo -> ResourcePath -> IO LByteString
produce loc ciInfo info (ResourcePath resPath) =

    -- Everyhting produced should be removed.
    -- This may lead to /tmp exhaustion attack, unless /tmp or memory
    -- is big enough.
    withSystemTempDirectory "faircv" $ \tmpPath -> do
        -- Latex reads and writes in the same dir - lets isolate it.
        copyDirectory resPath tmpPath

        let theText = generate loc ciInfo info
        let input   = encodeUtf8 theText

        D.withCurrentDirectory tmpPath $ do
            let action
                    = runProcess
                    $ setStdin (byteStringInput input)  -- feed the latex doc in directly
                    $ setStdout byteStringOutput
                    $ "xelatex"

            -- LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right.
            --
            -- That's why. And there's no picture behind header if only 1 action is run.
            _ <- action
            _ <- action

            LBS.readFile "texput.pdf"

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
    , cfiGrades =
        [ CertificateGrade
            { cgGrade = minBound
            , cgCredits = Just 132
            , cgHours = 123
            , cgLang = RU
            , cgSubject = "Следование за обозом"
            }
        , CertificateGrade
            { cgGrade = maxBound
            , cgCredits = Nothing
            , cgHours = 13
            , cgLang = RU
            , cgSubject = "Черпание"
            }
        , CertificateGrade
            { cgGrade = minBound
            , cgCredits = Just 34
            , cgHours = 1
            , cgLang = EN
            , cgSubject = "Сопротивление холере"
            }
        ]
    }
