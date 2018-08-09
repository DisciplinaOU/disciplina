-- | Resources allocation helpers.

module Dscp.Resource.Functions
       ( handleComponentErrors
       , runComponentR
       , runResourceAllocation
       , exitSilentlyOnFailure
       ) where

import Control.Monad.Component (ComponentError (..), ComponentM, runComponentM)
import qualified Data.Text.Prettyprint.Doc as Doc (pretty)
import Fmt ((+||), (||+))
import Loot.Log (logError)

import Dscp.Launcher.Rio (runRIO)
import Dscp.Resource.Class (InitContext (..), InitParams (..))
import Dscp.Resource.Logging (allocLogging)

-- | Catches and prints possible synchronous exceptions thrown
-- in 'runComponentM'. This includes both resource allocation errors and
-- application errors.
handleComponentErrors :: IO a -> IO (Either ComponentError a)
handleComponentErrors action = do
    res <- try action
    whenLeft res $ print . Doc.pretty
    return res

allocInitResources :: InitParams -> ComponentM InitContext
allocInitResources InitParams{..} = do
    _icLogging <- allocLogging ipLoggingParams
    return InitContext{..}

-- | Similar to 'runComponentM', but allows to pass context.
runComponentR
    :: Text
    -> r
    -> ReaderT r ComponentM a
    -> (a -> IO b)
    -> IO b
runComponentR appName ctx component main =
    runComponentM appName (runReaderT component ctx) main

-- | Allocates initial context, runs 'ComponentM' with it and handles possible
-- exceptions.
runResourceAllocation
    :: Text
    -> InitParams
    -> ReaderT InitContext ComponentM a
    -> (a -> IO b)
    -> IO (Either ComponentError b)
runResourceAllocation desc params component main = do
    eres <- try $ runComponentM initDesc (allocInitResources params) $
      \initCtx -> do
         eres <- try $ runComponentR desc initCtx component main
         whenLeft eres $ \(err :: ComponentError) -> do
             runRIO initCtx $ logError (""+||Doc.pretty err||+"")
         either throwM pure eres
    whenLeft eres $ \case
        ComponentRuntimeFailed{ componentErrorOriginalException = cause }
            | isJust (fromException @ComponentError cause) ->
            -- already printed error above
            pass

        err ->
            -- have no logging context here, so printing as is
            print $ Doc.pretty err
    return eres
  where
    initDesc = "Preliminary resource allocation"

exitSilentlyOnFailure :: IO (Either e a) -> IO a
exitSilentlyOnFailure action = action >>= either (\_ -> exitFailure) return
