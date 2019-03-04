{-# LANGUAGE GADTs #-}

-- | Resources used by Educator node

module Dscp.MultiEducator.Launcher.Resource
       ( EducatorContexts
       , EducatorContextsVar (..)
       , LoadedEducatorContext (..)
       , MaybeLoadedEducatorContext (..)
       , MultiEducatorResources (..)
       , merWitnessResources
       , merEducatorData
       ) where

import Control.Lens (makeLenses)
import Loot.Log (logWarning)
import qualified Pdf.FromLatex as Pdf
import Time (sec)
import UnliftIO.Async (Async, forConcurrently_, cancel)

import Dscp.Config
import Dscp.DB.SQL (SQL)
import qualified Dscp.Educator.Config as E
import qualified Dscp.Educator.Launcher.Mode as E
import Dscp.MultiEducator.Config
import Dscp.Resource.Class (AllocResource (..), buildComponentR)
import Dscp.MultiEducator.Types
import Dscp.Resource.Network (NetServResources)
import Dscp.Util.TimeLimit
import Dscp.Util.HasLens
import qualified Dscp.Witness.Launcher.Resource as Witness

-- | Context and related stuff of a single educator.
data LoadedEducatorContext where
    LoadedEducatorContext
        :: E.HasEducatorConfig
        => { lecCtx :: E.EducatorContext
           , lecWorkerHandlers :: [Async ()]
           }
        -> LoadedEducatorContext

data MaybeLoadedEducatorContext
      -- | Educator is not yet loaded, please retry later.
    = YetLoadingEducatorContext
      -- | Educator context has been loaded.
    | FullyLoadedEducatorContext LoadedEducatorContext

type EducatorContexts = Map EducatorId MaybeLoadedEducatorContext

-- | Contexts of every loaded educator.
newtype EducatorContextsVar = EducatorContextsVar (TVar EducatorContexts)

-- | Datatype which contains resources required by all Disciplina nodes
-- to start working.
data MultiEducatorResources = MultiEducatorResources
    { _merWitnessResources :: !Witness.WitnessResources
    , _merDB               :: !SQL
    , _merEducatorData     :: !EducatorContextsVar
    , _merPdfLatexPath     :: !Pdf.LatexPath
    , _merPdfResourcePath  :: !Pdf.ResourcePath
    , _merDownloadBaseUrl  :: !Pdf.DownloadBaseUrl
    }

makeLenses ''MultiEducatorResources
deriveHasLensDirect ''MultiEducatorResources

deriveHasLens 'merWitnessResources ''MultiEducatorResources ''Witness.WitnessResources
deriveHasLens 'merWitnessResources ''MultiEducatorResources ''NetServResources

instance AllocResource EducatorContextsVar where
    type Deps EducatorContextsVar = ()
    allocResource () =
        fmap EducatorContextsVar $
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
                FullyLoadedEducatorContext ctx ->
                    forM_ (lecWorkerHandlers ctx) $
                        logWarningWaitInf (sec 1) "Educator worker shutdown" . cancel

instance AllocResource MultiEducatorResources where
    type Deps MultiEducatorResources = MultiEducatorConfigRec
    allocResource educatorCfg = do
        let cfg = educatorCfg ^. sub #educator
            witnessCfg = rcast educatorCfg
        _merWitnessResources <- withWitnessConfig witnessCfg $
                               allocResource witnessCfg
        _merDB <- allocResource $ cfg ^. sub #db
        let appDir = Witness._wrAppDir _merWitnessResources
        _merEducatorData <- allocResource ()
        _merPdfLatexPath <- allocResource $ cfg ^. sub #certificates . option #latex
        _merPdfResourcePath <- allocResource
            (cfg ^. sub #certificates . option #resources, appDir)
        _merDownloadBaseUrl <- allocResource $ cfg ^. sub #certificates . option #downloadBaseUrl
        return MultiEducatorResources {..}
