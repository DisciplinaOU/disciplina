{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists  #-}

module Test.Dscp.Educator.Bot.Endpoints where

import Control.Lens ((?~))
import Data.Default (def)
import Servant.Util ((?/~))

import Dscp.Config
import Dscp.Core.Arbitrary
import Dscp.Crypto
import Dscp.Educator.Web.Bot
import Dscp.Educator.Web.Student
import Dscp.Educator.Web.Types
import Dscp.Util.Test
import Test.QuickCheck.Monadic (pick)

import Test.Dscp.DB.SQL.Mode
import Test.Dscp.Educator.Mode

student :: Student
student = studentEx

studentSK :: SecretKey
studentSK = studentSKEx

testBotHandlers :: (BotWorkMode ctx m, HasBotSetting) => StudentApiHandlers m
testBotHandlers = addBotHandlers student (studentApiHandlers student)

testBotParams :: Text -> EducatorBotParamsRec
testBotParams seed = finaliseDeferredUnsafe $
    mempty
    & option #seed            ?~ seed
    & option #operationsDelay ?~ 0

spec_StudentApiWithBotQueries :: Spec
spec_StudentApiWithBotQueries = specWithTempPostgresServer $ do
    it "Student gets assigned on courses on first request" $
        educatorProperty $ \seed -> do
            StudentApiEndpoints{..} <- initializeBot (testBotParams seed) $
                                       testBotHandlers <$ botProvideInitSetting student
            courses <- sGetCourses False [#isEnrolled ?/~ True] def def
            return (not $ null courses)

    it "Student gets some assignments on first request" $
        educatorProperty $ \seed -> do
            StudentApiEndpoints{..} <- initializeBot (testBotParams seed) $
                                       testBotHandlers <$ botProvideInitSetting student
            assignments <- sGetAssignments False def def def
            return (not $ null assignments)

    it "Submissions are graded automatically" $
        educatorPropertyM $ do
            seed <- pick arbitrary
            env <- pickSmall $
                    genCoreTestEnv simpleCoreTestParams
                    { ctpSecretKey = oneTestItem (pure studentSK)
                    , ctpAssignment = oneTestItem (pure assignmentEx) }
            let sigsub = tiOne $ cteSignedSubmissions env
            submissions <- lift $
                initializeBot (testBotParams seed) $ do
                    let StudentApiEndpoints{..} = testBotHandlers
                    botProvideInitSetting student
                    void $ sAddSubmission (signedSubmissionToRequest sigsub)
                    sGetSubmissions False def def def
            [submission] <- pure submissions
            return (isJust $ siGrade submission)
