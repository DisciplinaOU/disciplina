-- | Bot data and aspects of behaviour.

module Dscp.Educator.Web.Bot.Setting
     ( BotWorkMode
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
-- Utils
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

---------------------------------------------------------------------
-- Constants
---------------------------------------------------------------------

data BotSeed = BotSeed { unBotSeed :: Int }
type HasBotSeed = Given BotSeed

withBotSeed :: Int -> (HasBotSeed => a) -> a
withBotSeed s = give (BotSeed s)

botGen :: HasBotSeed => Gen a -> a
botGen = detGen (unBotSeed given)

botDataSeed :: Int
botDataSeed = 234234234  -- TODO [DSCP-163] move to config

-- TODO: Fill subjects when private chain is made
botCourses :: [(Course, Text, [Id Subject])]
botCourses =
    [ (Course 11, "Basic math", [])
    , (Course 21, "Classic physics", [])

    , (Course 12, "Advanced math", [])
    , (Course 22, "Quantum physics", [])
    , (Course 23, "Sci-fi physics", [])
    , (Course 31, "You, yes YOU REVIEWER, coin some names for a couple more subjects", [])
    ]

-- | We show a small set of courses at the beginning in order not to
-- confuse user, and disclose all others later to prevent him getting bored.
botBasicCourses, botAdvancedCourses :: [Course]
(botBasicCourses, botAdvancedCourses) =
    splitAt 3 (botCourses ^.. traversed . _1)

genBotCourseAssignments :: Int -> Course -> Gen [Assignment]
genBotCourseAssignments n _aCourseId =
    forM [1..n] $ \i -> do
        _aContentsHash <- arbitrary
        let _aType = if i == n then CourseFinal else Regular
        let _aDesc = "Task #" <> pretty i
        return Assignment{..}

botCourseAssignments :: HasBotSeed => Map Course [WithDependencies Assignment]
botCourseAssignments =
    -- TODO [DSCP-163] Will be regenerated on each access, can be expensive
    M.fromList $
    botGen $
    forM (zip courseSizes botCourses) $ \(courseSize, (course, _, _)) -> do
        assignments <- genBotCourseAssignments courseSize course
        assignAndDeps <- genDependencies assignments
        return (course, assignAndDeps)
  where
    courseSizes = 3 : 5 : botGen (vectorOf 5 $ choose (1, 7))

botAssignments :: HasBotSeed => [Assignment]
botAssignments = map wdItem $ fold botCourseAssignments

---------------------------------------------------------------------
-- Operations
---------------------------------------------------------------------

type BotWorkMode m =
    ( MonadSQLiteDB m
    , MonadCatch m
    )

botPrepareInitialData :: BotWorkMode m => m ()
botPrepareInitialData =
    withBotSeed botDataSeed $ do
        mapM_ (\(c, t, s) -> createCourse c (Just t) s) botCourses
        mapM_ createAssignment botAssignments

-- REMEMBER that all operations should be no-throw

-- | Ignore "Already present" errors.
maybePresent :: MonadCatch m => m a -> m ()
maybePresent action = catchJust asAlreadyExistsError (void action) (\_ -> pass)

-- Bot does not delete entities, so "Not found" errors should not happen.

-- | Assign student on assignments which became available for student
-- since dependant assignments are completed.
botProvideUnlockedAssignments
    :: BotWorkMode m
    => Student -> Course -> Set Assignment -> m ()
botProvideUnlockedAssignments student course completedAssigns =
    withBotSeed botDataSeed $
        whenJust (M.lookup course botCourseAssignments) $ \courseAssigns ->
            forM_ courseAssigns $ \(WithDependencies assign deps) ->
                when (deps `S.isSubsetOf` completedAssigns) $
                    setStudentAssignment student (getId assign)

-- | Enroll student to given courses and give initial assignments.
botProvideCourses :: BotWorkMode m => Student -> [Course] -> m ()
botProvideCourses student courses = do
    forM_ courses $ \course -> do
        enrollStudentToCourse student course
        botProvideUnlockedAssignments student course mempty

-- | Remember student and add minimal set of courses.
botProvideInitSetting :: BotWorkMode m => Student -> m ()
botProvideInitSetting student = do
    maybePresent $ createStudent student
    botProvideCourses student botBasicCourses

-- | Only for the chosen ones.
botProvideAdvancedSetting :: BotWorkMode m => Student -> m ()
botProvideAdvancedSetting student = do
    botProvideCourses student botAdvancedCourses

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
