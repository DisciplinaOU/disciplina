-- | Bot data and aspects of behaviour.

module Dscp.Educator.Web.Bot.Setting
     ( BotSetting (..)
     , mkBotSetting
     , HasBotSetting
     , withBotSetting

     , botLog
     , delayed

     , BotWorkMode
     , botPrepareInitialData
     , botProvideInitSetting
     , botProvideAdvancedSetting
     , botGradeSubmission
     , botProvideUnlockedAssignments
     ) where

import Control.Exception.Safe (catchJust)
import qualified Data.Map as M
import Data.Reflection (Given (..), give)
import qualified Data.Set as S
import Data.Time.Clock (getCurrentTime)
import Fmt ((+|), (+||), (|+), (||+))
import qualified GHC.Exts as Exts
import Loot.Base.HasLens (HasCtx)
import Loot.Log (ModifyLogName, MonadLogging, logError, logInfo, modifyLogName)
import Time.Units (Microsecond, Time, threadDelay)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (async)

import Dscp.Core
import Dscp.Crypto.Impl
import Dscp.DB.SQLite
import Dscp.Educator.Web.Bot.Params
import Dscp.Educator.Web.Student.Types
import Dscp.Util
import Dscp.Util.Test
import Dscp.Witness.Instances ()

---------------------------------------------------------------------
-- Generation
---------------------------------------------------------------------

data WithDependencies a = WithDependencies
    { wdItem  :: a
    , _wdDeps :: Set (Hash a)
    } deriving (Show)

-- | For each item in list, assign each element on his left as its dependency
-- with 50% probability.
genDependencies :: (Ord a, HasHash a) => [a] -> Gen [WithDependencies a]
genDependencies items = do
    forM (zip items (inits items)) $ \(item, allBefore) -> do
        deps <- sublistOf allBefore
        return $ WithDependencies item (S.fromList $ map hash deps)

genBotCourseAssignments :: Int -> Course -> Gen [Assignment]
genBotCourseAssignments n _aCourseId =
    forM [1..n] $ \i -> do
        let _aContentsHash = offlineHash
        let _aType = if i == n then CourseFinal else Regular
        let _aDesc = if i == n then "Exam" else "Task #" <> pretty i
        return Assignment{..}

---------------------------------------------------------------------
-- Constants
---------------------------------------------------------------------

-- | Set of courses and other stuff educator provides.
-- Could be plain constants, but we want to generate them with seed.
-- Contains excessive data for convenience.
data BotSetting = BotSetting
    {
      -- | All courses info
      bsCourses           :: [(Course, Text, [Id Subject])]
      -- | We show a small set of courses at the beginning in order not to
      -- confuse user, and disclose all others later to prevent him getting
      -- bored.
    , bsBasicCourses      :: [Course]
    , bsAdvancedCourses   :: [Course]
      -- | All assignments info on per course basis.
    , bsCourseAssignments :: Map Course [WithDependencies Assignment]
      -- | Flattened assignments.
    , bsAssignments       :: [Assignment]
      -- | Artificial delay in bot operations.
    , bsOperationsDelay   :: Time Microsecond
    }

-- | Generate a bot setting.
mkBotSetting :: EducatorBotParams -> BotSetting
mkBotSetting params =
  BotSetting{ bsOperationsDelay = ebpOperationsDelay params, ..}
 where
  botGen :: Gen a -> a
  botGen = detGenG (ebpSeed params)

  bsCourses =
    -- using 'courseEx' here helps to keep examples in swagger doc working
    [ (Course 0  , "Patakology", [])
    , (Course 1  , "Learning!", [])
    , (courseEx  , "Boredom", [])

    , (Course 101, "Introduction To Basics", [])
    , (Course 102, "Principles of Intermediate", [])
    , (Course 103, "Studyology", [])
    , (Course 104, "Class 101", [])
    , (Course 105, "Theoretical Phys Ed", [])
    , (Course 106, "Math 1-2-3", [])
    , (Course 107, "Simplified Chinese", [])
    , (Course 108, "Reading ?", [])
    , (Course 109, "Nominal Ascertainment", [])
    , (Course 110, "Canned Response Awareness", [])
    , (Course 111, "Advanced Arts & Craft", [])

    , (Course 201, "History of something", [])
    , (Course 202, "Tap dance", [])
    , (Course 203, "Modern dance", [])
    , (Course 204, "Statistics", [])
    , (Course 205, "Accounting", [])
    , (Course 206, "Italian wine tasting", [])
    , (Course 207, "Spanish 101", [])
    , (Course 208, "Spanish 102", [])
    , (Course 209, "Anthropology", [])
    , (Course 210, "Biology", [])
    , (Course 211, "History", [])
    , (Course 212, "History of ice cream", [])
    , (Course 213, "Law", [])
    , (Course 214, "Criminology", [])
    , (Course 215, "Grifting 101", [])
    , (Course 216, "Sci-fi physics", [])
    ]

  (bsBasicCourses, bsAdvancedCourses) =
    splitAt 3 (map (view _1) bsCourses)

  bsCourseAssignments =
    M.fromList $
    botGen $
    forM (zip courseSizes bsCourses) $ \(courseSize, (course, _, _)) -> do
        assignments <- genBotCourseAssignments courseSize course
        let assignmentsWithEx =
                bool identity (assignmentEx :)
                (course == _aCourseId assignmentEx)
                assignments
        assignAndDeps <- genDependencies assignmentsWithEx
        return (course, assignAndDeps)
    where
      courseSizes = 2 : 3 : botGen (vectorOf 5 $ choose (1, 7))

  bsAssignments = map wdItem $ fold bsCourseAssignments

type HasBotSetting = Given BotSetting

withBotSetting :: BotSetting -> (HasBotSetting => a) -> a
withBotSetting = give

botSetting :: HasBotSetting => BotSetting
botSetting = given

---------------------------------------------------------------------
-- Operations
---------------------------------------------------------------------

type BotWorkMode ctx m =
    ( MonadIO m
    , MonadCatch m
    , MonadUnliftIO m
    , MonadLogging m
    , ModifyLogName m
    , HasCtx ctx m '[SQLiteDB]
    )

botLog :: ModifyLogName m => m a -> m a
botLog = modifyLogName (<> "bot")

delayed :: (BotWorkMode ctx m, HasBotSetting) => m () -> m ()
delayed action
    | delay == 0 = action
    | otherwise =
        void . async $ do
            threadDelay delay
            action `catchAny` logException
  where
    delay = bsOperationsDelay botSetting
    logException e = botLog . logError $ "Delayed action failed: " +|| e ||+ ""

botPrepareInitialData :: (BotWorkMode ctx m, HasBotSetting) => m ()
botPrepareInitialData = transactW $ do
    exists <- existsCourse (head . Exts.fromList $ bsBasicCourses botSetting)
    unless exists $ do
        forM_ (bsCourses botSetting) $
            \(c, t, s) -> createCourse (CourseDetails (Just c) t s)
        mapM_ createAssignment (bsAssignments botSetting)

-- REMEMBER that all operations below should be no-throw

-- | Ignore "Already present" errors.
maybePresent :: MonadCatch m => m a -> m ()
maybePresent action = catchJust (^? _AlreadyPresentError) (void action) (\_ -> pass)

-- Bot does not delete entities, so "Not found" errors should not happen.

-- | Assign student on assignments which became available for student
-- since dependant assignments are completed.
botNoteCompletedAssignments
    :: (BotWorkMode ctx m, HasBotSetting)
    => Student -> Course -> Set (Hash Assignment) -> m ()
botNoteCompletedAssignments student course completedAssigns =
    whenJust (M.lookup course (bsCourseAssignments botSetting)) $
      \courseAssigns ->
        forM_ courseAssigns $ \(WithDependencies assign deps) ->
            when (deps `S.isSubsetOf` completedAssigns) $
                transactW $
                maybePresent $
                setStudentAssignment student (getId assign)

-- | Helper to invoke 'botNoteCompletedAssignments', accepts
-- student, made submission and overall list of student assignments.
botProvideUnlockedAssignments
    :: (BotWorkMode ctx m, HasBotSetting)
    => Student -> SubmissionStudentInfo -> [AssignmentStudentInfo] -> m ()
botProvideUnlockedAssignments student submission studentAssignments = do
    let assignment =
            fromMaybe (error "No related assignment among student ones") $
            find (\a -> siAssignmentHash submission == aiHash a)
                 studentAssignments
    let course = aiCourseId assignment
    let passedAssigns =
            S.fromList $
            map aiHash $
            filter (isJust . aiLastSubmission) studentAssignments
    botNoteCompletedAssignments student course passedAssigns

-- | Enroll student to given courses and give initial assignments.
botProvideCourses
    :: (BotWorkMode ctx m, HasBotSetting)
    => Student -> [Course] -> m ()
botProvideCourses student courses = do
    forM_ courses $ \course -> do
        transactW $ enrollStudentToCourse student course
        botNoteCompletedAssignments student course mempty

-- | Remember student and add minimal set of courses.
botProvideInitSetting :: (BotWorkMode ctx m, HasBotSetting) => Student -> m ()
botProvideInitSetting student = do
    maybePresent $ do
        void . invoke $ createStudent student
        botProvideCourses student (bsBasicCourses botSetting)
        botLog . logInfo $ "Registered student " +| student |+ ""

-- | Only for the chosen ones.
botProvideAdvancedSetting :: (BotWorkMode ctx m, HasBotSetting) => Student -> m ()
botProvideAdvancedSetting student = do
    maybePresent $ do
        botProvideCourses student (bsAdvancedCourses botSetting)
        botLog . logInfo $ "Student " +| student |+ " has discovered all courses"

-- | Add a grade immediatelly after student submission.
botGradeSubmission :: BotWorkMode ctx m => SignedSubmission -> m ()
botGradeSubmission ssub = do
    let sub = _ssSubmission ssub
        contentsH = _sContentsHash sub
    time <- liftIO getCurrentTime
    let grade = detGenG contentsH genPleasantGrade
    let ptx = PrivateTx
            { _ptSignedSubmission = ssub
            , _ptGrade = grade
            , _ptTime = time
            }
    maybePresent . transactW $ createTransaction ptx
