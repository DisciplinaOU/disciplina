module Test.Dscp.Educator.Bot.Endpoints where

import Dscp.Core.Arbitrary
import Dscp.Crypto
import Dscp.Educator.Web.Bot
import Dscp.Educator.Web.Student
import Dscp.Util.Test
import Test.QuickCheck.Monadic (pick)

import Test.Dscp.DB.SQLite.Common

student :: Student
student = studentEx

studentSK :: SecretKey
studentSK = studentSKEx

testMakeBotHandlers
    :: TestSQLiteM ~ m
    => Text -> m (StudentApiHandlers m)
testMakeBotHandlers seed =
    initializeBot
        EducatorBotParams
        { ebpEnabled = True
        , ebpSeed = seed
        , ebpOperationsDelay = 0
        } $ pure $ addBotHandlers student (studentApiHandlers student)

spec_StudentApiWithBotQueries :: Spec
spec_StudentApiWithBotQueries = describe "Basic properties" $ do
    it "Student gets assigned on courses on first request" $
        educatorProperty $ \seed -> do
            StudentApiEndpoints{..} <- testMakeBotHandlers seed
            courses <- sGetCourses Nothing False
            return (not $ null courses)

    it "Student gets some assignments on first request" $
        educatorProperty $ \seed -> do
            StudentApiEndpoints{..} <- testMakeBotHandlers seed
            assignments <- sGetAssignments Nothing Nothing Nothing False
            return (not $ null assignments)

    it "Submissions are graded automatically" $
        educatorPropertyM $ do
            seed <- pick arbitrary
            env <- pickSmall $
                    genCoreTestEnv simpleCoreTestParams
                    { ctpSecretKey = oneTestItem (pure studentSK)
                    , ctpAssignment = oneTestItem (pure assignmentEx) }
            let sigsub = tiOne $ cteSignedSubmissions env
            submissions <- lift $ do
                StudentApiEndpoints{..} <- testMakeBotHandlers seed
                void $ sAddSubmission (signedSubmissionToRequest sigsub)
                sGetSubmissions Nothing Nothing Nothing False
            [submission] <- pure submissions
            return (isJust $ siGrade submission)
