{- | Thread-safe educator context load.

This module is assumed to contain all the code which manipulates with contexts
state.


# Implementation notes:

For each educator context we have a thread which keeps resources, workers and
other stuff associated with the context; once this thread is killed,
corresponding context is destroyed.


Prepared context is put into `EducatorContextUsersVar` state.
In total, this state is updated 4 times during lifecycle of the context:

1. @Nothing -> Just LockedEducatorContext@ - happens when the first request
to the corresponding educator occurs. This lock is taken until the context
is ready to use.

2. @Just LockedEducatorContext -> Just FullyLoadedEducatorContext@ - the context
is ready and can be used by whoever needs it.

3. @Just FullyLoadedEducatorContext -> Just LockedEducatorContext@ - context
termination initiated. Further we kill all context users, release resources,
stop workers.

4. @Just LockedEducatorContext -> Nothing@ - context is fully removed, now
one another context can be created for this educator.


We account all context users (e.g. a thread corresponding to request to server)
so that when the context is terminated we perform an attempt to kill those users
gently (compare to suddenly getting error about closed database connection).

-}
module Dscp.MultiEducator.Launcher.Educator.Load
    ( MultiEducatorIsTerminating (..)
    , onTerminatedThrow
    , withSingleEducator
    , unloadEducator
    ) where

import qualified Control.Concurrent.STM as STM
import Control.Lens (at, traversed, zoom, (%=), _Just)
import Control.Monad.Fix (mfix)
import qualified Data.Set as S
import Fmt ((+|), (|+))
import Loot.Base.HasLens (lensOf)
import Loot.Log (MonadLogging, logDebug)
import Serokell.Util (modifyTVarS)
import Time (Timestamp, hour, sec, threadDelay)
import UnliftIO (Async, MonadUnliftIO, async, cancel, forConcurrently_, pollSTM, wait)

import qualified Dscp.Educator.Config as E
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
import Dscp.Util.Time
import Dscp.Util.TimeLimit

-- | Action which saves provided educator context while the provided action
-- lasts; once it completes, the context is unloaded.
type FillingEducatorContext m =
    forall a. E.HasEducatorConfig => E.EducatorContext -> m a -> m a

-- | Exception indicating that multi-educator is about to stop and does not
-- accept further requests.
-- TODO: consider it not as 500 internal on server?
data MultiEducatorIsTerminating = MultiEducatorIsTerminating
    deriving (Show)

instance Exception MultiEducatorIsTerminating

-- | Zoom into 'EducatorContext' expecting that multi-educator is active;
-- if it is terminating, a corresponding exception is thrown.
onTerminatedThrow :: StateT EducatorContextsMap STM a -> StateT EducatorContexts STM a
onTerminatedThrow action = StateT $ \case
    TerminatedEducatorContexts -> throwM MultiEducatorIsTerminating
    ActiveEducatorContexts ctxs -> second ActiveEducatorContexts <$> runStateT action ctxs

-- | Remember a thread using this context.
rememberUser :: Async a -> LoadedEducatorContext -> LoadedEducatorContext
rememberUser (void -> userAsync) = lecUsersL %~ S.insert userAsync

-- | Forget about a completed thread which used this context.
forgetUser :: MultiEducatorWorkMode ctx m => EducatorUUID -> Async a -> m ()
forgetUser educatorId (void -> actionAsync) = do
    educatorContexts <- view $ lensOf @MultiEducatorResources . merEducatorData

    atomically . modifyTVarS educatorContexts $
        -- if the context has been terminated, then we do not care about deleting,
        -- it is being done for us.
        zoom (_ActiveEducatorContexts . at educatorId) $
            -- for the same reasons we do nothing when the context is not loaded.
            _Just . _FullyLoadedEducatorContext . lecUsersL %=
                S.delete actionAsync

-- | Do our best to kill all user threads to perform resources cleanup
-- safely later.
disperseUsers
    :: (MonadUnliftIO m, MonadLogging m)
    => EducatorContextUsers -> m ()
disperseUsers users = do
    forConcurrently_ users $ \user -> do
        res <- boundExecution (sec 3) "Educator context user waiting" $ wait user
        whenNothing res $
            boundExecution_ (sec 1) "Educator context user termination" $ cancel user

-- | Reads a ready context from state or takes a lock if it does not exist yet.
-- If a context was present, we also update it in-place to reduce number of
-- state touches.
getContextOrLock
    :: MultiEducatorWorkMode ctx m
    => EducatorAuthLogin -> Timestamp -> Async a -> m (Maybe LoadedEducatorContext)
getContextOrLock educatorAuthLogin curTime userAsync = do
    educatorContexts <- view $ lensOf @MultiEducatorResources . merEducatorData
    let educatorId = ealId educatorAuthLogin

    atomically . modifyTVarS educatorContexts . onTerminatedThrow . zoom (at educatorId) $
        get >>= \case
            Nothing ->
                -- will load the context oursevles, taking a lock
                Nothing <$ put (Just LockedEducatorContext)

            Just LockedEducatorContext ->
                -- someone else has taken a lock
                lift STM.retry

            Just (FullyLoadedEducatorContext ctx) -> do
                -- already prepared
                ctx' <- updateOldCtx ctx
                put . Just $ FullyLoadedEducatorContext ctx'
                return (Just ctx')
  where
    -- Note: we need this because even if we already have the 'EducatorCtxWithCfg'
    -- in the map it may need to be updated with a new token
    updateOldCtx ctx = do
        let certIssuerInfoRes = makeCertIssuerRes educatorAuthLogin
            newEdCtx = lecCtx ctx & E.ecResources.E.erPdfCertIssuerRes .~ certIssuerInfoRes
        return $ ctx
            { lecCtx = newEdCtx
            , lecLastActivity = curTime
            } & rememberUser userAsync

-- | A couple for 'getContextOrLock', releases a previously taken lock leaving
-- no context in state for the given educator.
cleanLockedContext :: MultiEducatorWorkMode ctx m => EducatorUUID -> m ()
cleanLockedContext educatorId = do
    educatorContexts <- view $ lensOf @MultiEducatorResources . merEducatorData

    atomically . modifyTVarS educatorContexts $
        zoom (_ActiveEducatorContexts . at educatorId) $
            put Nothing

-- | Replace a lock with the prepared context, execute given action and replace
-- the context with the lock back cleaning resources kept in this context.
--
-- Generally this function does not guarantee that the action will be executed,
-- we may abort at context preparation stage.
fillingEducatorContext
    :: (MonadUnliftIO m, MonadMask m, MonadLogging m, E.HasEducatorConfig)
    => EducatorContextsVar
    -> EducatorUUID
    -> LoadedEducatorContext
    -> m a
    -> m a
fillingEducatorContext educatorContexts educatorId loadedCtx action = do
    bracket_ fillContext releaseContext action
  where
    fillContext =
        atomically . modifyTVarS educatorContexts $
            onTerminatedThrow . zoom (at educatorId) $
                put $ Just (FullyLoadedEducatorContext loadedCtx)

    releaseContext = do
        mctx <- atomically . modifyTVarS educatorContexts $
            zoom (_ActiveEducatorContexts . at educatorId) $
                get >>= \case
                    Just (FullyLoadedEducatorContext ctx) -> do
                        put $ Just LockedEducatorContext
                        return (Alt $ Just ctx)
                    _ -> error "Weird state"

        whenJust (getAlt mctx) $ \ctx ->
            disperseUsers (lecUsers ctx)

-- | Once a thread serving the context is forked, wait for that context to load
-- and return it once ready.
waitForContextLoad
    :: MultiEducatorWorkMode ctx m
    => EducatorUUID -> Async Void -> m LoadedEducatorContext
waitForContextLoad educatorId contextKeeper = do
    educatorContexts <- view $ lensOf @MultiEducatorResources . merEducatorData

    atomically . modifyTVarS educatorContexts . onTerminatedThrow . zoom (at educatorId) $ do
        mctx' <- get
        mterminated <- lift $ pollSTM contextKeeper
        case (mctx', mterminated) of
            (Just (FullyLoadedEducatorContext ctx), _) -> return ctx
            (_, Just term)                             -> either throwM absurd term
            (Just LockedEducatorContext, Nothing)      -> lift STM.retry
            -- context has been unloaded, need to read exception from the context keeper
            (Nothing, Nothing)                         -> lift STM.retry

-- | An action which is happening in the context keeping thread.
-- It allocates resources, launches workers and waits till async exception
-- after which everything is stoped/cleaned up.
serveEducatorContext
    :: MultiEducatorWorkMode ctx m
    => FillingEducatorContext E.EducatorRealMode
    -> EducatorAuthLogin
    -> m a
serveEducatorContext fillingContext educatorAuthLogin = do
    logDebug $ "Loading educator " +| educatorId |+ ""

    launchSingleEducatorMode educatorAuthLogin $ do
        educatorContext <- ask
        let singleEducatorClients =
                educatorClients & traversed . wIdL %~ (<> "_of_" <> encodeUtf8 eid)

        withWorkers singleEducatorClients $
            fillingContext educatorContext $
                forever $ threadDelay (hour 1)

            -- TODO: wrap each LoadedEducatorContext entry into own tvar for performance?
  where
    educatorId@(EducatorUUID eid) = ealId educatorAuthLogin

-- | Find a context in state. If it is not present, load a new context and return it.
lookupEducator
    :: MultiEducatorWorkMode ctx m
    => EducatorAuthLogin
    -> Async a
    -> m LoadedEducatorContext
lookupEducator educatorAuthLogin userAsync = do
    educatorContexts <- view $ lensOf @MultiEducatorResources . merEducatorData
    curTime <- getCurTime
    let educatorId = ealId educatorAuthLogin

    mask_ $ do
        mctx <- getContextOrLock educatorAuthLogin curTime userAsync

        whenNothing mctx $ do
            let fillingContext :: Async Void -> FillingEducatorContext E.EducatorRealMode
                fillingContext contextKeeper ctx =
                    fillingEducatorContext educatorContexts educatorId $
                        LoadedEducatorContext
                        { lecCtx = ctx
                        , lecLastActivity = curTime
                        , lecUsers = S.singleton (void userAsync)
                        , lecContextKeeper = contextKeeper
                        }

            contextKeeper <- mfix $ \contextKeeper ->
                async $ serveEducatorContext (fillingContext contextKeeper) educatorAuthLogin
                          `finally` (cleanLockedContext educatorId)

            waitForContextLoad educatorId contextKeeper

-- | Execute an action in educator context.
withSingleEducator
    :: MultiEducatorWorkMode ctx m
    => EducatorAuthLogin -> E.EducatorRealMode a -> m a
withSingleEducator educatorAuthLogin action = do
    bracket prepare terminate wait
  where
    prepare = mfix $ \actionAsync -> do
        ctx <- lookupEducator educatorAuthLogin actionAsync
        async $ runRIO (lecCtx ctx) action
    terminate actionAsync =
        forgetUser (ealId educatorAuthLogin) actionAsync

-- | Synchronously unload educator context waiting for requests to server
-- to complete and releasing all associated resources.
unloadEducator
    :: (MonadUnliftIO m, MonadLogging m)
    => LoadedEducatorContext -> m ()
unloadEducator ctx =
    logWarningWaitInf (sec 1) "Educator context shutdown" $
        cancel $ lecContextKeeper ctx
