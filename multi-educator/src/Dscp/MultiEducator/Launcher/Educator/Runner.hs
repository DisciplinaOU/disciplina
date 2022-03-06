module Dscp.MultiEducator.Launcher.Educator.Runner
    ( launchSingleEducatorMode
    ) where

import Universum

import Loot.Base.HasLens (lensOf)
import Loot.Log (LoggingIO)
import UnliftIO (UnliftIO (..), askUnliftIO)

import qualified Dscp.Educator.Config as E
import qualified Dscp.Educator.Launcher.Mode as E
import qualified Dscp.Educator.Launcher.Resource as E
import Dscp.MultiEducator.Launcher.Context
import Dscp.MultiEducator.Launcher.Educator.Resource
import Dscp.MultiEducator.Launcher.Mode
import Dscp.MultiEducator.Web.Educator.Auth (EducatorAuthData (..), EducatorAuthLogin (..))
import Dscp.Resource.Class
import Dscp.Resource.Functions
import Dscp.Rio

formSingleEducatorContext
    :: MultiEducatorWorkMode ctx m
    => E.EducatorResources -> m E.EducatorContext
formSingleEducatorContext _ecResources = pure E.EducatorContext{..}

launchSingleEducatorMode
    :: (MultiEducatorWorkMode ctx m)
    => EducatorAuthLogin
    -> (E.HasEducatorConfig => E.EducatorRealMode Void)
    -> m a
launchSingleEducatorMode educatorAuthLogin action = do
    meCtx <- ask
    let deps = (meCtx ^. lensOf @MultiEducatorResources, educatorAuthLogin)
    UnliftIO unliftIO <- askUnliftIO

    liftIO $ runComponentR runName (initCtx meCtx) (allocResource deps) $
        \(SingleEducatorResources resources) ->
            E.withEducatorConfig educatorFromMultiEducatorConfig $ unliftIO $ do
                ctx <- formSingleEducatorContext resources
                runRIO ctx (vacuous action)
  where
    educatorId = eadId $ ealData educatorAuthLogin
    runName = "Educator " <> show educatorId
    initCtx ctx = InitContext
        { _icLogging = ctx ^. lensOf @LoggingIO
        }
