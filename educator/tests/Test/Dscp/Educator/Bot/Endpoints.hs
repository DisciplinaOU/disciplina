{-# LANGUAGE OverloadedLabels #-}

module Test.Dscp.Educator.Bot.Endpoints where

import Control.Lens ((?~))
import Data.Default (def)

import Dscp.Config
import Dscp.Core.Arbitrary
import Dscp.Crypto
import Dscp.Educator.Web.Bot
import Dscp.Educator.Web.Student
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
            StudentApiEndpoints{..} <- initializeBot (testBotParams seed) (pure testBotHandlers)
            courses <- sGetCourses Nothing False def def
            return (not $ null courses)

    it "Student gets some assignments on first request" $
        educatorProperty $ \seed -> do
            StudentApiEndpoints{..} <- initializeBot (testBotParams seed) (pure testBotHandlers)
            assignments <- sGetAssignments Nothing Nothing Nothing False def def
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
                    sGetSubmissions Nothing Nothing Nothing False def def
            [submission] <- pure submissions
            return (isJust $ siGrade submission)
