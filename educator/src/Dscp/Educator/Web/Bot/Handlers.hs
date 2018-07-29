module Dscp.Educator.Web.Bot.Handlers
     ( addBotHandlers
     ) where

import qualified Dscp.Core.Types as Core
import Dscp.Crypto
import Dscp.Educator.Web.Bot.Setting
import Dscp.Educator.Web.Student

addBotHandlers
    :: forall m. BotWorkMode m
    => StudentApiHandlers m -> m (StudentApiHandlers m)
addBotHandlers StudentApiEndpoints{..} =
    -- TODO [DSCP-163] Take seed from config
    withBotSetting (mkBotSetting 2342342) $ do
        botPrepareInitialData
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

        , sMakeSubmission = \ssub -> do
            botProvideInitSetting oneGeek
            res <- sMakeSubmission ssub
            botGradeSubmission ssub

            -- TODO [DSCP-163] Logging
            -- cheat: on 3 submissions for the same assignment unlock all courses
            let assign = ssub ^. Core.ssSubmission
                               . Core.sAssignment
            courseSubs <- sGetSubmissions Nothing (Just $ hash assign) Nothing
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
