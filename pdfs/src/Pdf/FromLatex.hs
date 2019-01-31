
module Pdf.FromLatex where

import Control.Arrow ((&&&))
import Data.Functor.Contravariant (Contravariant (..))
import Data.Functor.Contravariant.Divisible (Divisible (..), Decidable (..), divided)
import Data.Time.Calendar
import qualified Data.ByteString as BS
import Text.Printer hiding ((<>))
import qualified Data.Text.Lazy.Builder as Text
import Path.IO
import Path
import System.Directory
import System.IO.Temp
import System.Process.Typed

import Dscp.Core.Foundation.Educator

escapeInLatex :: Text -> Text
escapeInLatex text = text

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

custom :: (a -> Text.Builder) -> MkLatex a
custom = MkLatex

ignore :: MkLatex a
ignore = contramap (const ()) conquer

split :: (a -> b) -> MkLatex b -> MkLatex a -> MkLatex a
split proj = divide (proj &&& id)

allThe :: MkLatex () -> MkLatex a -> MkLatex [a]
allThe sep maker = aux
  where
    aux = choose
        (\case
            [] -> Left ()
            x : xs -> Right (x, xs))
        ignore
        (divided (divide (id &&& const ()) maker sep) aux)

fullInfo :: MkLatex CertificateFullInfo
fullInfo = divide (cfiMeta &&& cfiGrades) meta courses
  where
    meta
        = split cmStudentName      studentName
        $ split cmStudentBirthDate studentBirthDate
        $ ignore
      where
        studentName      = custom $ command "Name"        . pure . shown
        studentBirthDate = custom $ command "DateOfBirth" . pure . formatted
          where
            formatted day = shown d <> "//" <> shown m <> "//" <> shown y
              where
                (y, m, d) = toGregorian day

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

shown :: Show x => x -> Text.Builder
shown = text . show

command :: Text.Builder -> [Text.Builder] -> Text.Builder
command name args = "\\" <> name <> mconcat (map (\arg -> "{" <> arg <> "}") args)

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

type Locale = Text

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

-- data Diploma = Diploma
--     { dEducatorUrl     :: Text
--     , dEducationPeriod :: (Int, Int)
--     , dDiplomaId       :: Text
--     , dDegreeLevel     :: Text
--     , dMajor           :: Text
--     , dMinor           :: Text
--     , dPartTime        :: Bool
--     }

-- data Course = Course
--     { cName        :: Text
--     , cLanguage    :: Text
--     , cHours       :: Int
--     , cEstcCredits :: Maybe Int
--     , cResult      :: Text
--     }

newtype ResourcePath = ResourcePath { unResourcePath :: FilePath }

produce :: Locale -> StudentInfo -> ResourcePath -> IO ByteString
produce loc info (ResourcePath resources) = do
    withSystemTempDirectory "faircv" $ \dir -> do
        resPath <- parseRelDir resources
        tmpPath <- parseAbsDir dir
        copyDirRecur resPath tmpPath

        let input = encodeUtf8 $ generate loc info
        withCurrentDirectory dir $ do
            let action =
                    runProcess
                    $ setStdin (byteStringInput input)
                    $ setStdout byteStringOutput
                    $ "xelatex"

            -- LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right.
            --
            -- That's why. And there's no picture behind header if only 1 action is run.
            _ <- action
            _ <- action

            BS.readFile "texput.pdf"
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
