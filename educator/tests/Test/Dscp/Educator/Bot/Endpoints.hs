module Test.Dscp.Educator.Bot.Endpoints where

import Dscp.Educator.Web
import Dscp.Util.Test

import Test.Dscp.DB.SQLite.Common

spec_StudentApiWithBotQueries :: Spec
spec_StudentApiWithBotQueries = describe "Basic properties" $ do
    it "Student gets assigned on courses on first request" $
        sqliteProperty $ \() -> do
            StudentApiEndpoints{..} <- addBotHandlers studentApiHandlers
            courses <- sGetCourses Nothing
            return (not $ null courses)

    it "Student gets some assignments on first request" $
        sqliteProperty $ \() -> do
            StudentApiEndpoints{..} <- addBotHandlers studentApiHandlers
            assignments <- sGetAssignments Nothing Nothing Nothing
            return (not $ null assignments)

    -- TODO [DSCP-141]: uncomment. If does not work, contact me (@martoon)
    -- Currently I can't pass specific student into endpoint.
    -- it "Submissions are graded automatically" $
    --     sqliteProperty $ \
    --       ( delayedGen (genStudentSignedSubmissions arbitrary)
    --         -> (student, sigsub :| _)
    --       ) -> do
    --         StudentApiEndpoints{..} <- addBotHandlers studentApiHandlers
    --         void $ sMakeSubmission student sigsub
    --         [submission] <- sGetSubmissions Nothing Nothing Nothing
    --         return (isJust $ sGrade submission)
