module Dscp.Educator.Web.Bot.Handlers
     ( addBotHandlers
     , initializeBot
     ) where

import Loot.Log (logInfo)

import Dscp.Core (Student)
import Dscp.Educator.Web.Bot.Params
import Dscp.Educator.Web.Bot.Setting
import Dscp.Educator.Web.Student
import Dscp.Educator.Web.Types

initializeBot
    :: BotWorkMode ctx m
    => EducatorBotParams -> (HasBotSetting => m a) -> m a
initializeBot params m = withBotSetting (mkBotSetting params) $ do
    botPrepareInitialData
    botLog $ logInfo "Educator bot initiated"
    m

addBotHandlers
    :: forall m ctx. (BotWorkMode ctx m, HasBotSetting)
    => Student -> StudentApiHandlers m -> StudentApiHandlers m
addBotHandlers student StudentApiEndpoints{..} = botEndpoints
  where
    botEndpoints :: (BotWorkMode ctx m, HasBotSetting) => StudentApiHandlers m
    botEndpoints = StudentApiEndpoints
        { sGetCourses = \isEnrolledF onlyCount -> do
            botProvideInitSetting student
            sGetCourses isEnrolledF onlyCount

        , sGetCourse = \course -> do
            botProvideInitSetting student
            sGetCourse course

        , sGetAssignments = \courseIdF docTypeF isFinalF onlyCount -> do
            botProvideInitSetting student
            sGetAssignments courseIdF docTypeF isFinalF onlyCount

        , sGetAssignment = \assignH -> do
            botProvideInitSetting student
            sGetAssignment assignH

        , sGetSubmissions = \courseF assignHF docTypeF onlyCount -> do
            botProvideInitSetting student
            sGetSubmissions courseF assignHF docTypeF onlyCount

        , sGetSubmission = \subH -> do
            botProvideInitSetting student
            sGetSubmission subH

        , sAddSubmission = \newSub -> do
            botProvideInitSetting student
            res <- sAddSubmission newSub

            delayed $ do
                requestToSignedSubmission newSub >>= botGradeSubmission

                allAssigns <- sGetAssignments Nothing Nothing Nothing False
                botProvideUnlockedAssignments student res allAssigns

            -- Easter egg: once a couple of courses is completed,
            -- unlock all courses
            -- Not doing this too early in order not to confuse the user.
            -- Frontend team can still unlock courses quickly if they need
            -- because assignments for first two courses are fixed disregard
            -- the seed.
            courses <- sGetCourses (Just $ IsEnrolled True) False
            when (length (filter ciIsFinished courses) >= 2) $
                botProvideAdvancedSetting student

            return res

        , sDeleteSubmission = \subH -> do
            botProvideInitSetting student
            sDeleteSubmission subH

        , sGetProofs = \sinceF onlyCount -> do
            botProvideInitSetting student
            sGetProofs sinceF onlyCount
        }
