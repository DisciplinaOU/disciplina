{- | Thread-safe educator context load.

This module is assumed to contain all the code which manipulates with contexts
state.


# Implementation notes:

For each educator context we have a thread which keeps resources, workers and
other stuff associated with the context; once this thread is killed,
corresponding context is destroyed.


Prepared context is put into `EducatorContextUsersVar` state.
In total, this state is updated 4 times during lifecycle of the context:

1. @Nothing -> Just YetLoadingEducatorContext@ - happens when the first request
to the corresponding educator occurs. This sort of lock is taken until the context
is ready to use.

2. @Just YetLoadingEducatorcontext -> Just FullyLoadedEducatorContext@ - the context
is ready and can be used by whoever needs it.

3. @Just FullyLoadedEducatorContext -> Just TerminatingEducatorContext@ - context
termination initiated. Further we kill all context users, release resources,
stop workers.

4. @Just TerminatingEducatorContext -> Nothing@ - context is fully removed, now
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
import Control.Lens (at, ix, traversed, zoom, (%=))
import Control.Monad.Fix (mfix)
import qualified Data.Set as S
import Fmt ((+|), (|+))
import Loot.Base.HasLens (lensOf)
import Loot.Log (MonadLogging, logDebug, logWarning)
import Serokell.Util (modifyTVarS)
import Time (Timestamp, hour, sec, threadDelay)
import UnliftIO (Async, MonadUnliftIO, async, cancelWith, forConcurrently_, pollSTM, wait,
                 withException)

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
    forall a. E.HasEducatorConfig => E.EducatorContext -> m Void -> m a

-- | Exception indicating that multi-educator is about to stop and does not
-- accept further requests.
-- TODO: consider it not as 500 internal on server?
data MultiEducatorIsTerminating = MultiEducatorIsTerminating
    deriving (Show)

instance Exception MultiEducatorIsTerminating

-- | Exception used to terminate the thread which served related context.
-- Carries termination parameters.
-- This is the only exception permitted for killing context keepers.
data TerminateContextKeeper = TerminateContextKeeper
    { tckUsersKillingException :: SomeException
    } deriving Show

instance Exception TerminateContextKeeper

-- | Zoom into 'EducatorContext' expecting that multi-educator is active;
-- if it is terminating, a corresponding exception is thrown.
onTerminatedThrow
    :: MonadThrow m
    => StateT EducatorContextsMap m a -> StateT EducatorContexts m a
onTerminatedThrow action = StateT $ \case
    TerminatedEducatorContexts -> throwM MultiEducatorIsTerminating
    ActiveEducatorContexts ctxs -> second ActiveEducatorContexts <$> runStateT action ctxs

-- | Zoom into 'EducatorContext' expecting that multi-educator is active;
-- if it is terminating, skip the action.
onTerminatedDoNothing
    :: (Monad m, Monoid a)
    => StateT EducatorContextsMap m a -> StateT EducatorContexts m a
onTerminatedDoNothing = zoom _ActiveEducatorContexts

-- | Remember a thread using this context.
rememberUser :: Async a -> LoadedEducatorContext -> LoadedEducatorContext
rememberUser (void -> userAsync) = lecUsersL %~ S.insert userAsync

-- | Forget about a completed thread which used this context.
forgetUser :: MultiEducatorWorkMode ctx m => EducatorUUID -> Async a -> m ()
forgetUser educatorId (void -> actionAsync) = do
    educatorContexts <- view $ lensOf @MultiEducatorResources . merEducatorData

    ctxs <- atomically $ readTVar educatorContexts
    -- If the context has been terminated, then we do not care about deleting,
    -- it is being done for us.
    -- For the same reasons we do nothing when the context is not (already) loaded.
    let mCtxVar = ctxs ^? _ActiveEducatorContexts . ix educatorId
    whenJust mCtxVar $ \ctxVar ->
        atomically . modifyTVarS ctxVar $
            _FullyLoadedEducatorContext . lecUsersL %= S.delete actionAsync

-- | Do our best to kill all user threads to perform resources cleanup
-- safely later.
-- This operation is synchronous, idempotent and thread-safe.
disperseUsers
    :: (MonadUnliftIO m, MonadLogging m)
    => EducatorContextUsers -> SomeException -> m ()
disperseUsers users exc = do
    forConcurrently_ users $ \user -> do
        res <-
            boundExecution (sec 3) "Educator context user waiting" $
            wait user
        whenNothing res $
            boundExecution_ (sec 1) "Educator context user termination" $
            cancelWith user exc

-- | Reads a ready context from state or creates a new one and takes a lock
-- if it does not exist yet (and returns @Left@ or @Right@ correspondingly).
-- If a context was present, we also update it in-place to reduce number of
-- state touches.
getContextOrLock
    :: MultiEducatorWorkMode ctx m
    => EducatorAuthLogin
    -> Timestamp
    -> Async a
    -> m (Either EducatorContextVar LoadedEducatorContext)
getContextOrLock educatorAuthLogin curTime userAsync = do
    educatorContexts <- view $ lensOf @MultiEducatorResources . merEducatorData
    let educatorId = ealId educatorAuthLogin

    atomically . modifyTVarS educatorContexts . onTerminatedThrow . zoom (at educatorId) $ do
        get >>= \case
            Nothing -> do
                -- will load the context oursevles, taking a lock
                var <- lift $ newTVar YetLoadingEducatorContext
                put (Just var)
                return (Left var)

            Just ctxVar -> lift $ do
                ctx <- readTVar ctxVar >>= \case
                    YetLoadingEducatorContext -> STM.retry
                    FullyLoadedEducatorContext ctx -> return ctx
                    TerminatingEducatorContext{} -> STM.retry

                when (lecNoFurtherUsers ctx) $ STM.retry

                let ctx' = updateOldCtx ctx
                writeTVar ctxVar (FullyLoadedEducatorContext ctx')
                return (Right ctx')
  where
    -- Note: we need this because even if we already have the 'EducatorCtxWithCfg'
    -- in the map it may need to be updated with a new token
    updateOldCtx ctx =
        let certIssuerInfoRes = makeCertIssuerRes educatorAuthLogin
            newEdCtx = lecCtx ctx & E.ecResources.E.erPdfCertIssuerRes .~ certIssuerInfoRes
        in ctx
            { lecCtx = newEdCtx
            , lecLastActivity = curTime
            } & rememberUser userAsync

-- | A couple for 'getContextOrLock', releases a previously taken lock leaving
-- no context in state for the given educator.
cleanLockedContext :: MultiEducatorWorkMode ctx m => EducatorUUID -> m ()
cleanLockedContext educatorId = do
    educatorContexts <- view $ lensOf @MultiEducatorResources . merEducatorData

    atomically . modifyTVarS educatorContexts $
        onTerminatedDoNothing . zoom (at educatorId) $
            put Nothing

-- | Replace a lock with the prepared context, execute given action and replace
-- the context with the lock back cleaning resources kept in this context.
--
-- Generally this function does not guarantee that the action will be executed,
-- we may abort at context preparation stage.
fillingEducatorContext
    :: (MonadUnliftIO m, MonadMask m, MonadLogging m, E.HasEducatorConfig)
    => EducatorContextVar
    -> LoadedEducatorContext
    -> m Void
    -> m a
fillingEducatorContext ctxVar loadedCtx action =
    mask $ \unmask -> vacuous $ do
        fillContext
        unmask action `withException` releaseContext
  where
    fillContext =
        atomically $ writeTVar ctxVar (FullyLoadedEducatorContext loadedCtx)

    releaseContext TerminateContextKeeper{..} = do
        mctx <- atomically . modifyTVarS ctxVar . state $ \case
            FullyLoadedEducatorContext ctx ->
                (Right ctx, TerminatingEducatorContext (lecContextKeeper ctx))
            st@YetLoadingEducatorContext ->
                (Left "loading", st)
            st@TerminatingEducatorContext{} ->
                (Left "terminating", st)

        case mctx of
            Right ctx ->
                disperseUsers (lecUsers ctx) tckUsersKillingException
            Left (st :: Text) ->
                logWarning $ "fillingEducatorContext: weird state " +| st |+ ""

-- | Once a thread serving the context is forked, wait for that context to load
-- and return it once ready.
waitForContextLoad
    :: MultiEducatorWorkMode ctx m
    => EducatorContextVar -> EducatorContextKeeper -> m LoadedEducatorContext
waitForContextLoad ctxVar (EducatorContextKeeper contextKeeper) = do
    atomically . modifyTVarS ctxVar $ do
        mctx <- get
        mterminated <- lift $ pollSTM contextKeeper
        case (mctx, mterminated) of
            (FullyLoadedEducatorContext ctx, _)     -> return ctx
            (_, Just term)                          -> either throwM absurd term
            (YetLoadingEducatorContext, Nothing)    -> lift STM.retry

            -- Educator context got killed very quickly (e.g. because
            -- multi-educator shutdown has been initialized), let's wait for
            -- an error from the context keeper thread.
            (TerminatingEducatorContext{}, Nothing) -> lift STM.retry

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
  where
    educatorId@(EducatorUUID eid) = ealId educatorAuthLogin

-- | Find a context in state. If it is not present, load a new context and return it.
lookupEducator
    :: MultiEducatorWorkMode ctx m
    => EducatorAuthLogin
    -> Async a
    -> m LoadedEducatorContext
lookupEducator educatorAuthLogin userAsync = do
    curTime <- getCurTime
    let educatorId = ealId educatorAuthLogin

    mask_ $
        getContextOrLock educatorAuthLogin curTime userAsync >>= \case
            Right ctx -> return ctx
            Left ctxVar -> do
                let fillingContext :: EducatorContextKeeper -> FillingEducatorContext E.EducatorRealMode
                    fillingContext contextKeeper ctx =
                        fillingEducatorContext ctxVar $
                            LoadedEducatorContext
                            { lecCtx = ctx
                            , lecLastActivity = curTime
                            , lecUsers = S.singleton (void userAsync)
                            , lecContextKeeper = contextKeeper
                            , lecNoFurtherUsers = False
                            }

                contextKeeper <- mfix $ \contextKeeper ->
                    fmap EducatorContextKeeper $
                    async $ serveEducatorContext (fillingContext contextKeeper) educatorAuthLogin
                              `finally` (cleanLockedContext educatorId)

                waitForContextLoad ctxVar contextKeeper

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
-- You have to provide an exception used to kill all the current users if
-- they are unresponsive.
unloadEducator
    :: (MonadUnliftIO m, MonadLogging m, Exception e)
    => e -> EducatorContextKeeper -> m ()
unloadEducator usersExc (EducatorContextKeeper contextKeeper) =
    logWarningWaitInf (sec 1) "Educator context shutdown" $
        cancelWith contextKeeper contextKeeperExc
  where
    contextKeeperExc = TerminateContextKeeper
        { tckUsersKillingException = SomeException usersExc
        }
