-- | Educator context load.
module Dscp.MultiEducator.Launcher.Educator.Load
    ( lookupEducator
    , loadEducator
    , normalToMulti
    ) where

import qualified Control.Concurrent.STM as STM
import Control.Lens (at, traversed, zoom)
import Fmt ((+|), (|+))
import Loot.Base.HasLens (lensOf)
import Loot.Log (logDebug)
import Serokell.Util (modifyTVarS)
import Time (hour, threadDelay)
import UnliftIO (async)

import qualified Dscp.Educator.Launcher.Mode as E
import qualified Dscp.Educator.Launcher.Resource as E
import Dscp.Educator.Workers
import Dscp.MultiEducator.Launcher.Context
import Dscp.MultiEducator.Launcher.Educator.Context
import Dscp.MultiEducator.Launcher.Educator.Resource
import Dscp.MultiEducator.Launcher.Educator.Runner
import Dscp.MultiEducator.Launcher.Mode
import Dscp.MultiEducator.Types
import Dscp.MultiEducator.Web.Educator.Auth
import Dscp.Network
import Dscp.Rio (runRIO)

-----------------------------------------------------------------------------
--- (Almost) Natural Transformation
-----------------------------------------------------------------------------

-- | This function transforms normal workmode into multi-workmode using login.
-- It create a new Educator context if login was not found
lookupEducator
    :: MultiEducatorWorkMode ctx m
    => EducatorAuthLogin
    -> m LoadedEducatorContext
lookupEducator educatorAuthLogin = do
    educatorContexts <- view $ lensOf @MultiEducatorResources . merEducatorData

    let educatorId = eadId $ ealData educatorAuthLogin
        withEducatorContextAtomically
            :: MonadIO m
            => StateT (Maybe MaybeLoadedEducatorContext) STM a -> m a
        withEducatorContextAtomically =
            atomically . modifyTVarS educatorContexts . zoom (at educatorId)

    mask_ $ do
        mctx <- withEducatorContextAtomically $ do
            get >>= \case
                Nothing ->
                    -- will load the context oursevles, taking a lock
                    Nothing <$ put (Just YetLoadingEducatorContext)

                Just YetLoadingEducatorContext ->
                    -- someone else has taken a lock
                    lift STM.retry

                Just (FullyLoadedEducatorContext ctx) ->
                    -- already prepared
                    return (Just ctx)

        case mctx of
            Nothing -> let rollback = withEducatorContextAtomically $ put Nothing
                in (`onException` rollback) $ do
                        ctx <- loadEducator educatorAuthLogin
                        withEducatorContextAtomically $
                            put $ Just (FullyLoadedEducatorContext ctx)
                        return ctx
            -- Note: we need this because even if we already have the 'EducatorCtxWithCfg'
            -- in the map it may need to be updated with a new token
            Just ctx -> do
                let certIssuerInfoRes = makeCertIssuerRes educatorAuthLogin
                let newEdCtx = lecCtx ctx & E.ecResources.E.erPdfCertIssuerRes .~ certIssuerInfoRes
                    newCtxWithCfg = ctx {lecCtx = newEdCtx}
                withEducatorContextAtomically $
                    put $ Just (FullyLoadedEducatorContext newCtxWithCfg)
                return newCtxWithCfg

loadEducator
    :: (MultiEducatorWorkMode ctx m)
    => EducatorAuthLogin
    -> m LoadedEducatorContext
loadEducator educatorAuthLogin  = do
    logDebug $ "Loading educator " +| educatorId |+ ""

    ctxVar <- newEmptyMVar

    -- we will not ignore this async once DSCP-494 is completed
    _ <- async $ launchSingleEducatorMode educatorAuthLogin $ do
        educatorContext <- ask
        let singleEducatorClients =
                educatorClients & traversed . wIdL %~ (<> "_of_" <> encodeUtf8 eid)

        withWorkers singleEducatorClients $ do
            putMVar ctxVar LoadedEducatorContext
                { lecCtx = educatorContext
                }
            forever $ threadDelay (hour 1)

    takeMVar ctxVar
  where
    educatorId@(EducatorUUID eid) = ealId educatorAuthLogin

normalToMulti :: MultiEducatorWorkMode ctx m => LoadedEducatorContext -> E.EducatorRealMode a -> m a
normalToMulti LoadedEducatorContext{..} = runRIO lecCtx
