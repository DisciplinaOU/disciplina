module Test.Dscp.Educator.Bot.Endpoints where

import Dscp.Core.Arbitrary
import Dscp.Educator.Web.Bot
import Dscp.Educator.Web.Student
import Dscp.Educator.Web.Types
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

    it "Submissions are graded automatically" $
        sqliteProperty $ \
          ( delayedGen
              (genStudentSignedSubmissions
                  (pure oneGeekSK)
                  (arbitrary <&> \s -> s{ _sAssignment = assignmentEx }))
            -> (_, sigsub :| _)
          ) -> do
            StudentApiEndpoints{..} <- addBotHandlers studentApiHandlers
            void $ sMakeSubmission (signedSubmissionToRequest sigsub)
            [submission] <- sGetSubmissions Nothing Nothing Nothing
            return (isJust $ siGrade submission)
