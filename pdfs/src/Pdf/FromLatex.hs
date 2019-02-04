
module Pdf.FromLatex
    ( produce
    , testData
    , ResourcePath(..)
    , Language (..)
    , CertificateFullInfo
    )
    where

import Control.Arrow ((&&&))
import Data.Char (isSpace)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Functor.Contravariant.Divisible (Divisible (..), Decidable (..), divided)
import qualified Data.Set as Set (fromList, member)
import Data.Time.Calendar
import qualified Data.ByteString as BS
import Text.Printer hiding ((<>))
import qualified Data.Text.Lazy.Builder as Text
import qualified Data.Text.Lazy as Text
import qualified Data.Text as Text.Strict
import Path.IO
import Path
import System.Directory
import System.IO.Temp
import System.Process.Typed

import Dscp.Core.Foundation.Educator

escapeInLatex :: Text -> Text
escapeInLatex = Text.Strict.pack . escape . Text.Strict.unpack
  where
    escape = \case
        '\\' : sp  : rest | isSpace   sp -> "\\textbackslash{ }"  ++ escape rest
        '\\' :       rest                -> "\\textbackslash{}"   ++ escape rest
        '^'  :       rest                -> "\\textasciicircum{}" ++ escape rest
        '~'  :       rest                -> "\\textasciitilde{}"  ++ escape rest
        sp   :       rest | isSpace   sp -> " "                   ++ escape rest
        ch   :       rest | isSimple  ch -> "\\" ++ [ch]          ++ escape rest
        '"'  : ru  : rest | isRussian ru -> "<<"                  ++ escape (ru : rest)
        ru   : '"' : rest | isRussian ru -> [ru] ++ ">>"          ++ escape rest
        '"'  : en  : rest | isEnglish en -> "``"                  ++ escape (en : rest)
        en   : '"' : rest | isEnglish en -> [en] ++ ['"']         ++ escape rest
        ch   :       rest                -> ch                     : escape rest
        []                               -> []
      where
        isSimple  ch = ch `Set.member` Set.fromList "{}&%$#_"
        isEnglish ch = ch `Set.member` Set.fromList (['A'.. 'Z'] ++ ['a'.. 'z'])
        isRussian ch = ch `Set.member` Set.fromList (['А'.. 'Я'] ++ ['а'.. 'я'])

data MkLatex a = MkLatex (a -> Text.Builder)

instance Contravariant MkLatex where
    contramap f (MkLatex printer) = MkLatex (printer . f)

instance Divisible MkLatex where
    divide splitter ~(MkLatex left) ~(MkLatex right) = MkLatex $ \a -> do
        let (l, r) = splitter a
        left l <> "\n" <> right r

    conquer = MkLatex (const "")

instance Decidable MkLatex where
    choose selector ~(MkLatex left) ~(MkLatex right) = MkLatex $
        either left right . selector

    lose _ = ignore

the :: Text.Builder -> MkLatex a
the txt = custom (const txt)

custom :: (a -> Text.Builder) -> MkLatex a
custom = MkLatex

ignore :: MkLatex a
ignore = contramap (const ()) conquer

split :: (a -> b) -> MkLatex b -> MkLatex a -> MkLatex a
split proj = divide (proj &&& id)

inBlock :: Text.Builder -> MkLatex a -> MkLatex a
inBlock name make
    = divide (const () &&& id) (the ("\\begin{" <> name <> "}"))
    $ divide (id       &&& id) make
    $ the ("\\end{" <> name <> "}")

allThe :: Show a => MkLatex () -> MkLatex a -> MkLatex [a]
allThe sep maker = choose
    (\case
        []     -> Left ()
        x : xs -> Right (x, xs))
    ignore
    (divided
        (divide (id &&& const ()) maker sep)
        (allThe sep maker))

generate :: Language -> CertificateFullInfo -> Text
generate lang cert = do
    Text.toStrict $ Text.toLazyText $ make (lang, cert)
  where
    MkLatex make = fullInfo

fullInfo :: MkLatex (Language, CertificateFullInfo)
fullInfo
    = divided language
    $ split cfiMeta personal
    $ inBlock "document"
        $ divide (cfiMeta &&& cfiGrades) meta courses
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
        $ split cmEducatorName              (command "section"     $ pure . shownDesc)
        $ inBlock "Diploma" diploma

    diploma
        = split (const ())                  (command "EducatorUrl"     $ const ["http://example.com/"])
        $ split (cmStartYear &&& cmEndYear) (command "EducationPeriod" $ showBoth)
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

shown :: Show x => x -> Text.Builder
shown = text . escapeInLatex . show

shownDesc :: ItemDesc -> Text.Builder
shownDesc = text . escapeInLatex . unItemDesc

command :: Text.Builder -> (a -> [Text.Builder]) -> MkLatex a
command name prepare = MkLatex $ \a ->
    "\\"
    <> name
    <> mconcat
        (map
            (\arg -> "{" <> arg <> "}")
            (prepare a))

newtype ResourcePath = ResourcePath { unResourcePath :: FilePath }

produce :: Language -> CertificateFullInfo -> ResourcePath -> IO ByteString
produce loc info (ResourcePath resources) = do
    withSystemTempDirectory "faircv" $ \dir -> do
        resPath <- parseRelDir resources
        tmpPath <- parseAbsDir dir

        copyDirRecur resPath tmpPath

        let theText = generate loc info
        let input   = encodeUtf8 theText

        writeFile "tmp.tex" theText

        withCurrentDirectory dir $ do
            let action
                    = runProcess
                    $ setStdin (byteStringInput input)
                    $ setStdout byteStringOutput
                    $ "xelatex"

            -- LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right.
            --
            -- That's why. And there's no picture behind header if only 1 action is run.
            _ <- action
            _ <- action

            BS.readFile "texput.pdf"

testData :: CertificateFullInfo
testData = CertificateFullInfo
    { cfiMeta = CertificateMeta
        { cmSpecialization   = Just "\"Владение ~ черпаком & ведром\""
        , cmMajor            = "123 Черпание\\dropTable{\"students\"}"
        , cmTitle            = "Младший\nпомошник\rстаршего\tчерпальщика\\ \\\\ "
        , cmNumber           = 100500
        , cmEducationForm    = Parttime
        , cmEducatorName     = "Абыр Валг"
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
