-- | Resources used by multi-educator node.
module Dscp.MultiEducator.Launcher.Resource
       (
       ) where

import Loot.Log (logWarning)
import UnliftIO.Async (forConcurrently_)

import Dscp.Config
import Dscp.DB.SQL
import Dscp.MultiEducator.Config
import Dscp.MultiEducator.Launcher.Context
import Dscp.MultiEducator.Launcher.Educator.Context
import Dscp.Resource.Class (AllocResource (..), buildComponentR)
import qualified Dscp.Witness.Launcher.Resource as Witness

instance AllocResource EducatorContextsVar where
    type Deps EducatorContextsVar = ()
    allocResource () =
        buildComponentR "Educator contexts var"
            (newTVarIO mempty)
            shutdownAllContexts
      where
        shutdownAllContexts ctxsVar = do
            ctxs <- atomically $ readTVar ctxsVar
            -- TODO [DSCP-494]: prevent further insertions to the context var

            forConcurrently_ ctxs $ \case
                YetLoadingEducatorContext ->
                    logWarning "Educator context is being loaded during shutdown."
                    -- TODO [DSCP-494] Do something smarter
                FullyLoadedEducatorContext _ctx ->
                    -- TODO [DSCP-494] Terminate normally
                    pass

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
