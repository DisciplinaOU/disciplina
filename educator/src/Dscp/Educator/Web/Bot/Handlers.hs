module Dscp.Educator.Web.Bot.Handlers
     ( addBotHandlers
     , initializeBot
     ) where

import Loot.Log (logInfo)

import Dscp.Core (Student)
import Dscp.Educator.Web.Bot.Params
import Dscp.Educator.Web.Bot.Setting
import Dscp.Educator.Web.Student


initializeBot
    :: forall m a. BotWorkMode m
    => EducatorBotParams -> (HasBotSetting => m a) -> m a
initializeBot params m = withBotSetting (mkBotSetting params) $ do
    botPrepareInitialData
    botLog $ logInfo "Educator bot initiated"
    m

addBotHandlers
    :: forall m. (BotWorkMode m, HasBotSetting)
    => Student -> StudentApiHandlers m -> StudentApiHandlers m
addBotHandlers student StudentApiEndpoints{..} = botEndpoints
  where
    botEndpoints :: (BotWorkMode m, HasBotSetting) => StudentApiHandlers m
    botEndpoints = StudentApiEndpoints
        { sGetCourses = \isEnrolledF -> do
            botProvideInitSetting student
            sGetCourses isEnrolledF

        , sGetCourse = \course -> do
            botProvideInitSetting student
            sGetCourse course

        , sGetAssignments = \courseIdF docTypeF isFinalF -> do
            botProvideInitSetting student
            sGetAssignments courseIdF docTypeF isFinalF

        , sGetAssignment = \assignH -> do
            botProvideInitSetting student
            sGetAssignment assignH

        , sGetSubmissions = \courseF assignHF docTypeF -> do
            botProvideInitSetting student
            sGetSubmissions courseF assignHF docTypeF

        , sGetSubmission = \subH -> do
            botProvideInitSetting student
            sGetSubmission subH

        , sMakeSubmission = \newSub -> do
            botProvideInitSetting student
            res <- sMakeSubmission newSub

            delayed $ requestToSignedSubmission newSub >>= botGradeSubmission

            allAssigns <- sGetAssignments Nothing Nothing Nothing
            botProvideUnlockedAssignments student res allAssigns

            -- cheat: on 3 submissions for the same assignment unlock all courses
            let assignH = nsAssignmentHash newSub
            courseSubs <- sGetSubmissions Nothing (Just assignH) Nothing
            -- remembering about race conditions
            when (length courseSubs `elem` [3..4]) $
                botProvideAdvancedSetting student
            return res

        , sDeleteSubmission = \subH -> do
            botProvideInitSetting student
            sDeleteSubmission subH

        , sGetProofs = \sinceF -> do
            botProvideInitSetting student
            sGetProofs sinceF
        }
