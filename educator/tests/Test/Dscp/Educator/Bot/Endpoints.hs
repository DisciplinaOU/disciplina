module Test.Dscp.Educator.Bot.Endpoints where

import Dscp.Core.Arbitrary
import Dscp.Educator.Web.Bot
import Dscp.Educator.Web.Student
import Dscp.Util.Test

import Test.Dscp.DB.SQLite.Common

testMakeBotHandlers
    :: TestSQLiteM ~ m
    => Text -> m (StudentApiHandlers m)
testMakeBotHandlers seed =
    addBotHandlers
        EducatorBotParams
        { ebpSeed = seed
        , ebpOperationsDelay = 0
        }
        studentApiHandlers

spec_StudentApiWithBotQueries :: Spec
spec_StudentApiWithBotQueries = describe "Basic properties" $ do
    it "Student gets assigned on courses on first request" $
        sqliteProperty $ \seed -> do
            StudentApiEndpoints{..} <- testMakeBotHandlers seed
            courses <- sGetCourses Nothing
            return (not $ null courses)

    it "Student gets some assignments on first request" $
        sqliteProperty $ \seed -> do
            StudentApiEndpoints{..} <- testMakeBotHandlers seed
            assignments <- sGetAssignments Nothing Nothing Nothing
            return (not $ null assignments)

    it "Submissions are graded automatically" $
        sqliteProperty $ \
          ( seed
          , delayedGen
            (genCoreTestEnv simpleCoreTestParams
                            { ctpSecretKey = oneTestItem (pure oneGeekSK)
                            , ctpAssignment = oneTestItem (pure assignmentEx) })
             -> env
          ) -> do
            let sigsub = tiOne $ cteSignedSubmissions env
            StudentApiEndpoints{..} <- testMakeBotHandlers seed
            void $ sMakeSubmission (signedSubmissionToRequest sigsub)
            [submission] <- sGetSubmissions Nothing Nothing Nothing
            return (isJust $ siGrade submission)
