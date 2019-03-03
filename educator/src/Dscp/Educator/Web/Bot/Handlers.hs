module Dscp.Educator.Web.Bot.Handlers
     ( addBotHandlers
     , initializeBot
     ) where

import Data.Default (def)
import Loot.Log (logInfo)

import Dscp.Core (Student)
import Dscp.Educator.Web.Bot.Params
import Dscp.Educator.Web.Bot.Setting
import Dscp.Educator.Web.Student
import Dscp.Educator.Web.Types

initializeBot
    :: BotWorkMode ctx m
    => EducatorBotParamsRec -> (HasBotSetting => m a) -> m a
initializeBot params m = withBotSetting (mkBotSetting params) $ do
    botPrepareInitialData
    botLog $ logInfo "Educator bot initiated"
    m

-- | Make bot notice all related events like new submission of a student.
-- This does not include actions which should happen on first activity of each
-- student ('botProvideInitSettings'), which should be handled elsewhere (for instance,
-- in autentification hook).
addBotHandlers
    :: forall m ctx. (BotWorkMode ctx m, HasBotSetting)
    => Student -> StudentApiHandlers m -> StudentApiHandlers m
addBotHandlers student endpoints@StudentApiEndpoints{..} =
    endpoints
        { sAddSubmission = \newSub -> do
            res <- sAddSubmission newSub

            delayed $ do
                requestToSignedSubmission newSub >>= botGradeSubmission

                allAssigns <- sGetAssignments Nothing Nothing Nothing False def def
                botProvideUnlockedAssignments student res allAssigns

            -- Easter egg: once a couple of courses is completed,
            -- unlock all courses
            -- Not doing this too early in order not to confuse the user.
            -- Frontend team can still unlock courses quickly if they need
            -- because assignments for first two courses are fixed disregard
            -- the seed.
            courses <- sGetCourses (Just $ IsEnrolled True) False def def
            when (length (filter ciIsFinished courses) >= 2) $
                botProvideAdvancedSetting student

            return res
        }
