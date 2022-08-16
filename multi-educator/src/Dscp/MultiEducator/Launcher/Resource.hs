-- | Resources used by multi-educator node.
module Dscp.MultiEducator.Launcher.Resource
       (
       ) where

import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TVar (swapTVar)
import Loot.Log (logWarning)
import UnliftIO.Async (forConcurrently_)

import Dscp.Config
import Dscp.DB.SQL
import Dscp.MultiEducator.Config
import Dscp.MultiEducator.Launcher.Context
import Dscp.MultiEducator.Launcher.Educator.Context
import Dscp.MultiEducator.Launcher.Educator.Load
import Dscp.Resource.Class (AllocResource (..), buildComponentR)
import qualified Dscp.Witness.Launcher.Resource as Witness

instance AllocResource EducatorContextsVar where
    type Deps EducatorContextsVar = ()
    allocResource () =
        buildComponentR "Educator contexts var"
            newEducatorContexts
            shutdownAllContexts
      where
        newEducatorContexts = newTVarIO $ ActiveEducatorContexts mempty
        shutdownAllContexts ctxsVar =
            atomically (swapTVar ctxsVar TerminatedEducatorContexts) >>= \case
                TerminatedEducatorContexts ->
                    logWarning "Extra attempt to terminate all educator contexts"

                ActiveEducatorContexts ctxs ->
                    forConcurrently_ ctxs $ \ctxVar -> do
                        contextKeeper <- atomically $ readTVar ctxVar >>= \case
                            YetLoadingEducatorContext -> STM.retry
                            FullyLoadedEducatorContext ctx -> return (lecContextKeeper ctx)
                            TerminatingEducatorContext contextKeeper -> return contextKeeper
                        unloadEducator MultiEducatorIsTerminating contextKeeper

instance AllocResource MultiEducatorResources where
    type Deps MultiEducatorResources = MultiEducatorConfigRec
    allocResource educatorCfg = do
        let cfg = educatorCfg ^. sub #educator
            witnessCfg = rcast educatorCfg
        _merWitnessResources <- withWitnessConfig witnessCfg $
                               allocResource witnessCfg
        _merDB <- unPreparedSQL @"educator" <$> allocResource (cfg ^. sub #db)
        let appDir = Witness._wrAppDir _merWitnessResources
        _merEducatorData <- allocResource ()
        _merLanguage <- allocResource $ cfg ^. sub #certificates . option #language
        _merPdfLatexPath <- allocResource $ cfg ^. sub #certificates . option #latex
        _merPdfResourcePath <- allocResource
            (cfg ^. sub #certificates . option #resources, appDir)
        _merDownloadBaseUrl <- allocResource $ cfg ^. sub #certificates . option #downloadBaseUrl
        return MultiEducatorResources {..}
