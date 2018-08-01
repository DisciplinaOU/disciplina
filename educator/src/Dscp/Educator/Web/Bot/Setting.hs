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
import Control.Lens (traversed)
import qualified Data.Map as M
import Data.Reflection (Given (..), give)
import qualified Data.Set as S
import Data.Time.Clock (getCurrentTime)
import Dscp.Core.Foundation.Educator.Txs.Type
import Fmt ((+|), (+||), (|+), (||+))
import Loot.Log (ModifyLogName, MonadLogging, logError, logInfo, modifyLogName)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (async)
import UnliftIO.Concurrent (threadDelay)

import Dscp.Core.Arbitrary
import Dscp.Core.Types
import Dscp.Crypto.Impl
import Dscp.DB.SQLite
import qualified Dscp.Educator.Web.Student.Types as Student
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
    }

-- | Generate a bot setting.
mkBotSetting :: Int -> BotSetting
mkBotSetting seed = BotSetting{..}
 where
  botGen :: Gen a -> a
  botGen = detGen seed

  bsCourses =
    [ (courseEx , "Basic maths", [])
    , (Course 21, "Classic physics", [])

    , (Course 12, "Advanced math", [])
    , (Course 22, "Quantum physics", [])
    , (Course 23, "Sci-fi physics", [])
    , (Course 31, "English", [])
    , (Course 41, "Data structures", [])
    , (Course 42, "Discrete maths", [])
    , (Course 43, "Electronic computer architecture", [])
    ]

  (bsBasicCourses, bsAdvancedCourses) =
    splitAt 3 (bsCourses ^.. traversed . _1)

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
      courseSizes = 2 : 5 : botGen (vectorOf 5 $ choose (1, 7))

  bsAssignments = map wdItem $ fold bsCourseAssignments

type HasBotSetting = Given BotSetting

withBotSetting :: BotSetting -> (HasBotSetting => a) -> a
withBotSetting = give

botSetting :: HasBotSetting => BotSetting
botSetting = given

---------------------------------------------------------------------
-- Operations
---------------------------------------------------------------------

type BotWorkMode m =
    ( MonadSQLiteDB m
    , MonadCatch m
    , MonadUnliftIO m
    , MonadLogging m
    , ModifyLogName m
    )

botLog :: ModifyLogName m => m a -> m a
botLog = modifyLogName (<> "bot")

delayed :: (BotWorkMode m) => m () -> m ()
delayed action
    | botOpsDelay == 0 = action
    | otherwise =
        void . async $ do
            threadDelay botOpsDelay
            action `catchAny` logException
  where
    -- TODO [DSCP-163]: Take value from config
    -- keeping it 0 now for the sake of tests
    botOpsDelay = 0
    logException e = botLog . logError $ "Delayed action failed: " +|| e ||+ ""

botPrepareInitialData :: (BotWorkMode m, HasBotSetting) => m ()
botPrepareInitialData = do
    mapM_ (\(c, t, s) -> createCourse c (Just t) s) (bsCourses botSetting)
    mapM_ createAssignment (bsAssignments botSetting)

-- REMEMBER that all operations below should be no-throw

-- | Ignore "Already present" errors.
maybePresent :: MonadCatch m => m a -> m ()
maybePresent action = catchJust asAlreadyExistsError (void action) (\_ -> pass)

-- Bot does not delete entities, so "Not found" errors should not happen.

-- | Assign student on assignments which became available for student
-- since dependant assignments are completed.
botNoteCompletedAssignments
    :: (BotWorkMode m, HasBotSetting)
    => Student -> Course -> Set (Hash Assignment) -> m ()
botNoteCompletedAssignments student course completedAssigns =
    whenJust (M.lookup course (bsCourseAssignments botSetting)) $
      \courseAssigns ->
        forM_ courseAssigns $ \(WithDependencies assign deps) ->
            when (deps `S.isSubsetOf` completedAssigns) $
                maybePresent $
                setStudentAssignment student (getId assign)

-- | Helper to invoke 'botNoteCompletedAssignments', accepts
-- student, made submission and overall list of student assignments.
botProvideUnlockedAssignments
    :: (BotWorkMode m, HasBotSetting)
    => Student -> Student.Submission -> [Student.Assignment] -> m ()
botProvideUnlockedAssignments student submission studentAssignments = do
    let assignment =
            fromMaybe (error "No related assignment among student ones") $
            find (\a -> Student.sAssignmentHash submission == Student.aHash a)
                 studentAssignments
    let course = Student.aCourseId assignment
    let passedAssigns =
            S.fromList $
            map Student.aHash $
            filter (isJust . Student.aLastSubmission) studentAssignments
    botNoteCompletedAssignments student course passedAssigns

-- | Enroll student to given courses and give initial assignments.
botProvideCourses
    :: (BotWorkMode m, HasBotSetting)
    => Student -> [Course] -> m ()
botProvideCourses student courses = do
    forM_ courses $ \course -> do
        enrollStudentToCourse student course
        botNoteCompletedAssignments student course mempty

-- | Remember student and add minimal set of courses.
botProvideInitSetting :: (BotWorkMode m, HasBotSetting) => Student -> m ()
botProvideInitSetting student = do
    maybePresent $ do
        void $ createStudent student
        botProvideCourses student (bsBasicCourses botSetting)
        botLog . logInfo $ "Registered student " +| student |+ ""

-- | Only for the chosen ones.
botProvideAdvancedSetting :: (BotWorkMode m, HasBotSetting) => Student -> m ()
botProvideAdvancedSetting student = do
    botProvideCourses student (bsAdvancedCourses botSetting)
    botLog . logInfo $ "Student " +| student |+ " has discovered all courses"

-- | Add a grade immediatelly after student submission.
botGradeSubmission :: BotWorkMode m => SignedSubmission -> m ()
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
    maybePresent $ createTransaction ptx
