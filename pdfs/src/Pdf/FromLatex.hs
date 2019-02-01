
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
        '\\' : sp  : rest | isSpace   sp -> "\\textbackslash{ }" ++ escape rest
        '\\' :       rest                -> "\\textbackslash"    ++ escape rest
        '^'  :       rest                -> "\textasciicircum{}" ++ escape rest
        '~'  :       rest                -> "\textasciitilde{}"  ++ escape rest
        sp   :       rest | isSpace   sp -> " "                  ++ escape rest
        ch   :       rest | isSimple  ch -> "\\" ++ [ch]         ++ escape rest
        '"'  : ru  : rest | isRussian ru -> "<<"                 ++ escape (ru : rest)
        ru   : '"' : rest | isRussian ru -> [ru] ++ ">>"         ++ escape rest
        '"'  : en  : rest | isEnglish en -> "``"                 ++ escape (en : rest)
        en   : '"' : rest | isEnglish en -> [en] ++ ['"']        ++ escape rest
        ch   :       rest                -> ch                    : escape rest
        []                               -> []
      where
        isSimple  ch = ch `Set.member` Set.fromList "{}&%$#_"
        isEnglish ch = ch `Set.member` Set.fromList (['A'.. 'Z'] ++ ['a'.. 'z'])
        isRussian ch = ch `Set.member` Set.fromList (['А'.. 'Я'] ++ ['а'.. 'я'])

data MkLatex a = MkLatex (a -> Text.Builder)

instance Contravariant MkLatex where
    contramap f (MkLatex printer) = MkLatex (printer . f)

instance Divisible MkLatex where
    divide splitter (MkLatex left) (MkLatex right) = MkLatex $ \a -> do
        let (l, r) = splitter a
        left l <> "\n" <> right r

    conquer = MkLatex (const "")

instance Decidable MkLatex where
    choose selector (MkLatex left) (MkLatex right) = MkLatex $
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

allThe :: MkLatex () -> MkLatex a -> MkLatex [a]
allThe sep maker = aux
  where
    aux = choose
        (\case
            [] -> Left ()
            x : xs -> Right (x, xs))
        ignore
        (divided (divide (id &&& const ()) maker sep) aux)

generate :: Language -> CertificateFullInfo -> Text
generate lang cert = do
    Text.toStrict $ Text.toLazyText $ make (lang, cert)
  where
    MkLatex make = fullInfo

fullInfo :: MkLatex (Language, CertificateFullInfo)
fullInfo
    = divided language
    $ divide (cfiMeta &&& cfiGrades) meta courses
  where
    language = choose (\case RU -> Left (); EN -> Right ())
        (command "documentclass[11pt, russian]" $ const [text "faircv"])
        (command "documentclass[11pt, english]" $ const [text "faircv"])

    meta
        = split cmStudentName               (command "Name"        $ pure . shown)
        $ split cmStudentBirthDate          (command "DateOfBirth" $ formatDate)
        $ inBlock "Diploma" diploma

    diploma
        = split (const ())                  (command "EducatorUrl"     $ pure . shown)
        $ split (cmStartYear &&& cmEndYear) (command "EducationPeriod" $ showBoth)
        $ split  cmNumber                   (command "DiplomaId"       $ pure . shown)
        $ split  cmIssueDate                (command "DateOfIssue"     $ formatDate)
        $ split  cmTitle                    (command "DegreeLevel"     $ pure . shown)
        $ split  cmMajor                    (command "Major"           $ pure . shown)
        $ split  cmSpecialization           (command "Minor"           $ pure . shown)
        $ split  cmEducationForm             educationForm
        $ ignore

    educationForm = choose
        (\case
            Parttime -> Left ()
            _        -> Right ())
        (command "PartTimeEducation" (const []))
        ignore

    courses = allThe (custom $ const "\\\\") course
      where
        course = custom $ \CertificateGrade
            { cgSubject
            , cgLang
            , cgHours
            , cgCredits
            , cgGrade
            } -> ""
                <> shown cgSubject <> " & "
                <> shown cgLang    <> " & "
                <> shown cgHours   <> " & "
                <> maybe "---" shown cgCredits <> " & "
                <> shown cgGrade

    formatDate day = [shown d, shown m, shown y]
      where
        (y, m, d) = toGregorian day

    showBoth (a, b) = [shown a, shown b]

shown :: Show x => x -> Text.Builder
shown = text . escapeInLatex . show

command :: Text.Builder -> (a -> [Text.Builder]) -> MkLatex a
command name prepare = MkLatex $ \a ->
    "\\"
    <> name
    <> mconcat
        (map
            (\arg -> "{" <> arg <> "}")
            (prepare a))

{-
-- | Datatype containing info about a certificate issued by Educator.
data CertificateMeta = CertificateMeta
    { cmStudentName      :: !ItemDesc
    , cmStudentBirthDate :: !Day
    , cmStartYear        :: !Int
    , cmEndYear          :: !Int
    , cmEducationForm    :: !EducationForm
    , cmNumber           :: !Integer
    , cmIssueDate        :: !Day
    , cmTitle            :: !ItemDesc
    , cmMajor            :: !ItemDesc
    , cmSpecialization   :: !(Maybe ItemDesc)
    } deriving (Show, Eq, Generic)

data EducationForm = Fulltime | Parttime | Fullpart
    deriving (Show, Eq, Generic, Enum, Bounded)
-}

-- data Date = Date
--     { year  :: Int
--     , month :: Int
--     , day   :: Int
--     }

-- dateToString :: Locale -> Date -> Text
-- dateToString _ (Date y m d) = show d <> "." <> show m <> "." <> show y

-- data StudentInfo = StudentInfo
--     { siName        :: Text
--     , siDateOfBirth :: Date
--     , siSections    :: [Section]
--     }

-- data Section = Section
--     { sEducator :: Text
--     , sDiploma  :: Diploma
--     , sCourses  :: [Course]
--     }

-- data Course = Course
--     { cName        :: Text
--     , cLanguage    :: Text
--     , cHours       :: Int
--     , cEstcCredits :: Maybe Int
--     , cResult      :: Text
--     }

newtype ResourcePath = ResourcePath { unResourcePath :: FilePath }

produce :: Language -> CertificateFullInfo -> ResourcePath -> IO ByteString
produce loc info (ResourcePath resources) = do
    putStrLn ("md /tmp/dir..." :: Text)
    withSystemTempDirectory "faircv" $ \dir -> do
        putStrLn ("cp res /tmp/dir..." :: Text)
        resPath <- parseRelDir resources
        tmpPath <- parseAbsDir dir
        copyDirRecur resPath tmpPath

        let theText = generate loc info
        let input   = encodeUtf8 theText
        putStrLn ("echo tmp.tex" :: Text)
        writeFile "tmp.tex" theText
        putStrLn ("cd /tmp/dir..." :: Text)
        withCurrentDirectory dir $ do
            let action =
                    runProcess
                    $ setStdin (byteStringInput input)
                    $ setStdout byteStringOutput
                    $ "xelatex"

            -- LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right.
            --
            -- That's why. And there's no picture behind header if only 1 action is run.
            putStrLn ("xelatex /tmp/dir..." :: Text)
            _ <- action
            putStrLn ("xelatex /tmp/dir..." :: Text)
            _ <- action

            putStrLn ("Done!" :: Text)
            BS.readFile "texput.pdf"

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

{-
testData :: StudentInfo
testData = StudentInfo
    { siName        = "Фу Бар"
    , siDateOfBirth = Date 1990 5 13
    , siSections    =
        [ Section
            { sEducator = "Random dude"
            , sDiploma  = Diploma
                { dEducatorUrl     = "http://example.com"
                , dEducationPeriod = (2010, 2015)
                , dDiplomaId       = "Фу-Бар-42, 2015"
                , dDegreeLevel     = "Младший помошник старшего черпальщика"
                , dMajor           = "123 Черпание"
                , dMinor           = "Владение черпаком"
                , dPartTime        = True
                }
            , sCourses =
                [ Course
                    { cName        = "Следование за обозом"
                    , cLanguage    = "Latin"
                    , cHours       = 420
                    , cEstcCredits = Nothing
                    , cResult      = "Устал"
                    }
                , Course
                    { cName        = "Черпание"
                    , cLanguage    = "POSIX"
                    , cHours       = 666
                    , cEstcCredits = Just 55
                    , cResult      = "Пролил"
                    }
                , Course
                    { cName        = "Сопротивление холере"
                    , cLanguage    = "С"
                    , cHours       = 2
                    , cEstcCredits = Just 8
                    , cResult      = "Провалил"
                    }
                ]
            }
        ]
    }
-}
-- generate :: Locale -> StudentInfo -> Text
-- generate
--     locale
--     StudentInfo
--         { siName
--         , siDateOfBirth
--         , siSections
--         }
--   = run
--   $ ""
--     `nl` command ("documentclass[11pt," <> locale <> "]") ["faircv"]
--     `nl` command "Name"        [siName]
--     `nl` command "DateOfBirth" [dateToString locale siDateOfBirth]
--     `nl` block   "document" (""
--         `nl` command "MakeHeader" []
--         `nl` unlinesBuilder (map generateSection siSections)
--     )

-- unlinesBuilder :: [Builder] -> Builder
-- unlinesBuilder = foldr slashN ""
--   where
--     slashN start rest = start <> "\n" <> rest

-- generateSection :: Section -> Builder
-- generateSection
--     Section
--         { sEducator
--         , sDiploma
--         , sCourses
--         }
--   = ""
--     `nl` command "section" [sEducator]
--     `nl` block   "Diploma" (generateDiploma sDiploma)
--     `nl` block   "Courses" (unlinesBuilder (map generateCourse sCourses))

-- generateDiploma :: Diploma -> Builder
-- generateDiploma
--     Diploma
--         { dEducatorUrl
--         , dEducationPeriod = (from, to)
--         , dDiplomaId
--         , dDegreeLevel
--         , dMajor
--         , dMinor
--         , dPartTime
--         }
--   = ""
--     `nl` command "EducatorUrl"     [dEducatorUrl]
--     `nl` command "EducationPeriod" [show from, show to]
--     `nl` command "DiplomaId"       [dDiplomaId]
--     `nl` command "DegreeLevel"     [dDegreeLevel]
--     `nl` command "Major"           [dMajor]
--     `nl` command "Minor"           [dMinor]
--     `nl` (if dPartTime then command0 "PartTimeEducation" else "")

-- generateCourse :: Course -> Builder
-- generateCourse
--     Course
--         { cName
--         , cLanguage
--         , cHours
--         , cEstcCredits
--         , cResult
--         }
--   = ""
--     <>  fromText cName
--     -&- fromText cLanguage
--     -&- fromText (show cHours)
--     -&- fromText (maybe "---" show cEstcCredits)
--     -&- fromText cResult
--     <>  " \\\\"

-- infixr 6 `nl`
-- nl :: (IsString a, Semigroup a) => Builder -> Builder -> a
-- a `nl` b = a <> "\n" <> b

-- infixr 6 -&-
-- (-&-) :: (IsString a, Semigroup a) => a -> a -> a
-- a -&- b = a <> " & " <> b

-- command :: Text -> [Text] -> Builder
-- command commName params
--     =  fromText (command0 commName)
--     <> mconcat  (map (\text -> "{" <> fromText (escapeInLatex text) <> "}") params)

-- command0 :: Text -> Builder
-- command0 commName = "\\" <> fromText commName

-- block :: Text -> Builder -> Builder
-- block name text
--     = ""
--     `nl` command "begin" [name]
--     `nl` text
--     `nl` command "end" [name]
