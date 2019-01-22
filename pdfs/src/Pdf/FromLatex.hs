
module Pdf.FromLatex where

import System.IO.Temp
import System.Process

escapeInLatex :: Text -> Text
escapeInLatex text = text

type Locale = Text

data Date = Date
    { year  :: Int
    , month :: Int
    , day   :: Int
    }

dateToString :: Locale -> Date -> Text
dateToString _ (Date y m d) = show d <> "." <> show m <> "." <> show y

data StudentInfo = StudentInfo
    { siName        :: Text
    , siDateOfBirth :: Date
    , siSections    :: [Section]
    }

data Section = Section
    { sEducator :: Text
    , sDiploma  :: Diploma
    , sCourses  :: [Course]
    }

data Diploma = Diploma
    { dEducatorUrl     :: Text
    , dEducationPeriod :: (Int, Int)
    , dDiplomaId       :: Text
    , dDegreeLevel     :: Text
    , dMajor           :: Text
    , dMinor           :: Text
    , dPartTime        :: Bool
    }

data Course = Course
    { cName        :: Text
    , cLanguage    :: Text
    , cHours       :: Int
    , cEstcCredits :: Maybe Int
    , cResult      :: Text
    }

newtype ResourcePath = ResourcePath { unResourcePath :: FilePath }

convert :: Locale -> ResourcePath -> IO ByteString
convert loc resources tmp = do
    withSystemTempFile "faircv" $ \fname _handle -> do
        runProcess "xelatex" []

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

generate :: Locale -> StudentInfo -> Text
generate
    locale
    StudentInfo
        { siName
        , siDateOfBirth
        , siSections
        }
  = ""
    `nl` command ("documentclass[11pt," <> locale <> "]") ["faircv"]
    `nl` command "Name"        [siName]
    `nl` command "DateOfBirth" [dateToString locale siDateOfBirth]
    `nl` block   "document" (""
        `nl` command "MakeHeader" []
        `nl` unlines (map generateSection siSections)
    )

generateSection :: Section -> Text
generateSection
    Section
        { sEducator
        , sDiploma
        , sCourses
        }
  = ""
    `nl` command "Section" [sEducator]
    `nl` block   "Diploma" (generateDiploma sDiploma)
    `nl` block   "Courses" (unlines (map generateCourse sCourses))

generateDiploma :: Diploma -> Text
generateDiploma
    Diploma
        { dEducatorUrl
        , dEducationPeriod = (from, to)
        , dDiplomaId
        , dDegreeLevel
        , dMajor
        , dMinor
        , dPartTime
        }
  = ""
    `nl` command "EducatorUrl"     [dEducatorUrl]
    `nl` command "EducationPeriod" [show from, show to]
    `nl` command "DiplomaId"       [dDiplomaId]
    `nl` command "DegreeLevel"     [dDegreeLevel]
    `nl` command "Major"           [dMajor]
    `nl` command "Minor"           [dMinor]
    `nl` (if dPartTime then command0 "PartTimeEducation" else "")

generateCourse :: Course -> Text
generateCourse
    Course
        { cName
        , cLanguage
        , cHours
        , cEstcCredits
        , cResult
        }
  = ""
    <>  cName
    -&- cLanguage
    -&- show cHours
    -&- maybe "---" show cEstcCredits
    -&- cResult
    <>  "//"

nl :: (IsString a, Semigroup a) => a -> a -> a
a `nl` b = a <> "\n" <> b

(-&-) :: (IsString a, Semigroup a) => a -> a -> a
a -&- b = a <> " & " <> b

command :: Text -> [Text] -> Text
command commName params = command0 commName <> mconcat (map (\text -> "{" <> escapeInLatex text <> "}") params)

command0 :: Text -> Text
command0 commName = "\\" <> commName

block :: Text -> Text -> Text
block name text
    = ""
    `nl` command "begin" [name]
    `nl` text
    `nl` command "end" [name]
