-- | Bot data and aspects of behaviour.

module Dscp.Educator.Web.Bot.Setting
     ( BotSetting (..)
     , mkBotSetting
     , HasBotSetting
     , withBotSetting

     , BotWorkMode
     , botPrepareInitialData
     , botProvideInitSetting
     , botProvideAdvancedSetting
     , botGradeSubmission
     ) where

import Control.Exception.Safe (catchJust)
import Control.Lens (traversed)
import qualified Data.Map as M
import Data.Reflection (Given (..), give)
import qualified Data.Set as S
import Data.Time.Clock (getCurrentTime)

import Dscp.Core.Arbitrary (genPleasantGrade)
import Dscp.Core.Types
import Dscp.DB.SQLite
import Dscp.Educator.Txs
import Dscp.Util
import Dscp.Util.Test
import Dscp.Witness.Instances ()

---------------------------------------------------------------------
-- Generation
---------------------------------------------------------------------

data WithDependencies a = WithDependencies
    { wdItem  :: a
    , _wdDeps :: Set a
    }

-- | For each item in list, assign each element on his left as its dependency
-- with 50% probability.
genDependencies :: Ord a => [a] -> Gen [WithDependencies a]
genDependencies items = do
    forM (zip items (inits items)) $ \(item, allBefore) -> do
        deps <- sublistOf allBefore
        return $ WithDependencies item (S.fromList deps)

genBotCourseAssignments :: Int -> Course -> Gen [Assignment]
genBotCourseAssignments n _aCourseId =
    forM [1..n] $ \i -> do
        _aContentsHash <- arbitrary
        let _aType = if i == n then CourseFinal else Regular
        let _aDesc = "Task #" <> pretty i
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
      bsCourses :: [(Course, Text, [Id Subject])]
      -- | We show a small set of courses at the beginning in order not to
      -- confuse user, and disclose all others later to prevent him getting
      -- bored.
    , bsBasicCourses
    , bsAdvancedCourses :: [Course]
      -- | All assignments info on per course basis.
    , bsCourseAssignments :: Map Course [WithDependencies Assignment]
      -- | Flattened assignments.
    , bsAssignments :: [Assignment]
    }

-- | Generate a bot setting.
mkBotSetting :: Int -> BotSetting
mkBotSetting seed = BotSetting{..}
 where
  botGen :: Gen a -> a
  botGen = detGen seed

  bsCourses =
    [ (Course 11, "Basic math", [])
    , (Course 21, "Classic physics", [])

    , (Course 12, "Advanced math", [])
    , (Course 22, "Quantum physics", [])
    , (Course 23, "Sci-fi physics", [])
    , (Course 31, "You, yes YOU REVIEWER, coin some names for a couple more subjects", [])
    ]

  (bsBasicCourses, bsAdvancedCourses) =
    splitAt 3 (bsCourses ^.. traversed . _1)

  bsCourseAssignments =
    M.fromList $
    botGen $
    forM (zip courseSizes bsCourses) $ \(courseSize, (course, _, _)) -> do
        assignments <- genBotCourseAssignments courseSize course
        assignAndDeps <- genDependencies assignments
        return (course, assignAndDeps)
    where
      courseSizes = 3 : 5 : botGen (vectorOf 5 $ choose (1, 7))

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
    )

botPrepareInitialData :: (BotWorkMode m, HasBotSetting) => m ()
botPrepareInitialData = do
    mapM_ (\(c, t, s) -> createCourse c (Just t) s) (bsCourses botSetting)
    mapM_ createAssignment (bsAssignments botSetting)

-- REMEMBER that all operations should be no-throw

-- | Ignore "Already present" errors.
maybePresent :: MonadCatch m => m a -> m ()
maybePresent action = catchJust asAlreadyExistsError (void action) (\_ -> pass)

-- Bot does not delete entities, so "Not found" errors should not happen.

-- | Assign student on assignments which became available for student
-- since dependant assignments are completed.
botProvideUnlockedAssignments
    :: (BotWorkMode m, HasBotSetting)
    => Student -> Course -> Set Assignment -> m ()
botProvideUnlockedAssignments student course completedAssigns =
    whenJust (M.lookup course (bsCourseAssignments botSetting)) $
      \courseAssigns ->
        forM_ courseAssigns $ \(WithDependencies assign deps) ->
            when (deps `S.isSubsetOf` completedAssigns) $
                setStudentAssignment student (getId assign)

-- | Enroll student to given courses and give initial assignments.
botProvideCourses
    :: (BotWorkMode m, HasBotSetting)
    => Student -> [Course] -> m ()
botProvideCourses student courses = do
    forM_ courses $ \course -> do
        enrollStudentToCourse student course
        botProvideUnlockedAssignments student course mempty

-- | Remember student and add minimal set of courses.
botProvideInitSetting :: (BotWorkMode m, HasBotSetting) => Student -> m ()
botProvideInitSetting student = do
    maybePresent $ createStudent student
    botProvideCourses student (bsBasicCourses botSetting)

-- | Only for the chosen ones.
botProvideAdvancedSetting :: (BotWorkMode m, HasBotSetting) => Student -> m ()
botProvideAdvancedSetting student = do
    botProvideCourses student (bsAdvancedCourses botSetting)

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
