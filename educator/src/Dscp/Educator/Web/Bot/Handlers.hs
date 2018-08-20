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

            delayed $ do
                requestToSignedSubmission newSub >>= botGradeSubmission

                allAssigns <- sGetAssignments Nothing Nothing Nothing
                botProvideUnlockedAssignments student res allAssigns

            -- Easter egg: once a couple of courses is completed,
            -- unlock all courses
            -- Not doing this too early in order not to confuse the user.
            -- Frontend team can still unlock courses quickly if they need
            -- because assignments for first two courses are fixed disregard
            -- the seed.
            courses <- sGetCourses (Just $ IsEnrolled True)
            when (length (filter ciIsFinished courses) >= 2) $
                botProvideAdvancedSetting student

            return res

        , sDeleteSubmission = \subH -> do
            botProvideInitSetting student
            sDeleteSubmission subH

        , sGetProofs = \sinceF -> do
            botProvideInitSetting student
            sGetProofs sinceF
        }
