module Dscp.Educator.Web.Bot.Handlers
     ( addBotHandlers
     ) where

import Loot.Log (logInfo)

import Dscp.Educator.Web.Bot.Params
import Dscp.Educator.Web.Bot.Setting
import Dscp.Educator.Web.Student

addBotHandlers
    :: forall m. BotWorkMode m
    => EducatorBotParams -> StudentApiHandlers m -> m (StudentApiHandlers m)
addBotHandlers params StudentApiEndpoints{..} =
    withBotSetting (mkBotSetting params) $ do
        botPrepareInitialData
        botLog $ logInfo "Educator bot initiated"
        return botEndpoints
  where
    botEndpoints :: (BotWorkMode m, HasBotSetting) => StudentApiHandlers m
    botEndpoints = StudentApiEndpoints
        { sGetCourses = \isEnrolledF -> do
            botProvideInitSetting oneGeek
            sGetCourses isEnrolledF

        , sGetCourse = \course -> do
            botProvideInitSetting oneGeek
            sGetCourse course

        , sGetAssignments = \courseIdF docTypeF isFinalF -> do
            botProvideInitSetting oneGeek
            sGetAssignments courseIdF docTypeF isFinalF

        , sGetAssignment = \assignH -> do
            botProvideInitSetting oneGeek
            sGetAssignment assignH

        , sGetSubmissions = \courseF assignHF docTypeF -> do
            botProvideInitSetting oneGeek
            sGetSubmissions courseF assignHF docTypeF

        , sGetSubmission = \subH -> do
            botProvideInitSetting oneGeek
            sGetSubmission subH

        , sMakeSubmission = \newSub -> do
            botProvideInitSetting oneGeek
            res <- sMakeSubmission newSub

            delayed $ requestToSignedSubmission newSub >>= botGradeSubmission

            allAssigns <- sGetAssignments Nothing Nothing Nothing
            botProvideUnlockedAssignments oneGeek res allAssigns

            -- cheat: on 3 submissions for the same assignment unlock all courses
            let assignH = nsAssignmentHash newSub
            courseSubs <- sGetSubmissions Nothing (Just assignH) Nothing
            -- remembering about race conditions
            when (length courseSubs `elem` [3..4]) $
                botProvideAdvancedSetting oneGeek
            return res

        , sDeleteSubmission = \subH -> do
            botProvideInitSetting oneGeek
            sDeleteSubmission subH

        , sGetProofs = \sinceF -> do
            botProvideInitSetting oneGeek
            sGetProofs sinceF
        }
