{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell  #-}

-- | Module contains the definition of Educator's WorkMode and its implementations.

module Dscp.MultiEducator.Launcher.Mode
    (
      -- * Markers
      EducatorNode

      -- * Constraints
    , MultiEducatorWorkMode
    , MultiCombinedWorkMode

      -- * Implementations
    , MultiEducatorContext (..)
    , MultiEducatorRealMode
    , mecWitnessVars
    , lookupEducator
    , loadEducator
    , normalToMulti
    ) where

import Control.Concurrent.STM (retry)
import Control.Lens (at, makeLenses, zoom, (?~), _Wrapped')
import Fmt ((+|), (|+))
import Loot.Base.HasLens (HasLens', lensOf)
import Loot.Config (option, sub)
import Loot.Log (logDebug)
import Loot.Network.Class (NetworkingCli, NetworkingServ)
import Loot.Network.ZMQ (ZmqTcp)
import qualified Pdf.FromLatex as Pdf
import Serokell.Util (modifyTVarS)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))
import UnliftIO (async)

import Dscp.Config
import Dscp.Crypto
import Dscp.DB.SQL
import qualified Dscp.Educator.Config as E
import Dscp.Educator.DB (prepareEducatorSchema)
import Dscp.Educator.Launcher.Marker (EducatorNode)
import qualified Dscp.Educator.Launcher.Mode as E
import qualified Dscp.Educator.Launcher.Resource as E
import Dscp.Educator.Workers
import Dscp.MultiEducator.Config
import Dscp.MultiEducator.Launcher.Params (MultiEducatorKeyParams (..))
import Dscp.MultiEducator.Launcher.Resource
import Dscp.Network
import Dscp.Resource.AppDir (AppDir)
import Dscp.Resource.Keys (linkStore)
import Dscp.Resource.Network
import Dscp.Rio (RIO, runRIO)
import Dscp.Util.HasLens
import qualified Dscp.Witness as W

---------------------------------------------------------------------
-- WorkMode class
---------------------------------------------------------------------

-- | Set of typeclasses which define capabilities of bare Educator node.
type MultiEducatorWorkMode ctx m =
    ( W.WitnessWorkMode ctx m

    , HasMultiEducatorConfig
    , HasLens' ctx (TVar EducatorContexts)
    , HasLens' ctx MultiEducatorResources
    , HasLens' ctx W.WitnessVariables

    , NetworkingCli ZmqTcp m
    , NetworkingServ ZmqTcp m
    )

-- | Set of typeclasses which define capabilities both of Educator and Witness.
type MultiCombinedWorkMode ctx m =
    ( MultiEducatorWorkMode ctx m
    , W.FullWitnessWorkMode ctx m
    )

---------------------------------------------------------------------
-- WorkMode implementation
---------------------------------------------------------------------

data MultiEducatorContext = MultiEducatorContext
    { _mecResources   :: !MultiEducatorResources
      -- ^ Resources, allocated from params.
    , _mecWitnessVars :: !W.WitnessVariables
    }

makeLenses ''MultiEducatorContext
deriveHasLensDirect ''MultiEducatorContext

type MultiEducatorRealMode = RIO MultiEducatorContext

---------------------------------------------------------------------
-- HasLens
---------------------------------------------------------------------

deriveHasLens 'mecResources ''MultiEducatorContext ''MultiEducatorResources
deriveHasLens 'mecResources ''MultiEducatorContext ''W.WitnessResources
deriveHasLens 'mecResources ''MultiEducatorContext ''NetServResources
deriveHasLens 'mecWitnessVars ''MultiEducatorContext ''W.WitnessVariables

----------------------------------------------------------------------------
-- (Almost) Natural Transformation
----------------------------------------------------------------------------

-- | This function transforms normal workmode into multi-workmode using login.
-- It returns Nothing is login was not found
lookupEducator :: MultiEducatorWorkMode ctx m => Text -> m LoadedEducatorContext
lookupEducator login = do
    educatorContexts <- view $ lensOf @MultiEducatorResources . merEducatorData

    let withEducatorContextAtomically
            :: MonadIO m
            => StateT (Maybe MaybeLoadedEducatorContext) STM a -> m a
        withEducatorContextAtomically =
            atomically . modifyTVarS educatorContexts . zoom (_Wrapped' . at login)

    mask_ $ do
        mctx <- withEducatorContextAtomically $ do
            get >>= \case
                Nothing ->
                    -- will load the context oursevles, taking a lock
                    Nothing <$ put (Just YetLoadingEducatorContext)

                Just YetLoadingEducatorContext ->
                    -- someone else has taken a lock
                    lift retry

                Just (FullyLoadedEducatorContext ctx) ->
                    -- already prepared
                    return (Just ctx)

        whenNothing mctx $
            let rollback = withEducatorContextAtomically $ put Nothing
            in (`onException` rollback) $ do
                    ctx <- loadEducator login Nothing
                    withEducatorContextAtomically $
                        put $ Just (FullyLoadedEducatorContext ctx)
                    return ctx

    -- TODO lookupEducator should probably take a Maybe PassPhrase
    -- to pass it to 'loadEducator'
    -- or the relationship with loadEducator should be different/removed

loadEducator :: (MultiEducatorWorkMode ctx m) => Text -> Maybe PassPhrase -> m LoadedEducatorContext
loadEducator login mpassphrase = do
    logDebug $ "Loading educator " +| login |+ ""
    -- TODO: add hashing
    appDir <- view $ lensOf @AppDir
    ctx <- ask
    let resources = ctx ^. lensOf @MultiEducatorResources
        (MultiEducatorKeyParams keyDir) = multiEducatorConfig ^. sub #educator . option #keys
        db = resources ^. lensOf @SQL
    -- set the DB schema name and create it if it's not ready
    setSchemaName db ("educator_" <> login)
    prepareEducatorSchema db
    liftIO $ createDirectoryIfMissing True keyDir
    -- read key from file and creates one if it does not exist yet
    let keyParams = finaliseDeferredUnsafe $ mempty
            & option #path       ?~ Just (keyDir </> toString login <.> "key")
            & option #genNew     ?~ True
            & option #passphrase ?~ mpassphrase
    key <- withCoreConfig (rcast multiEducatorConfig) $ linkStore keyParams appDir
    -- build up a new educator context with config
    let educatorResources = E.EducatorResources
            { _erWitnessResources = resources ^. lensOf @W.WitnessResources
            , _erKeys = key
            , _erDB = db
            , _erPdfLatexPath = ctx ^. lensOf @MultiEducatorResources . lensOf @Pdf.LatexPath
            , _erPdfResourcePath = ctx ^. lensOf @MultiEducatorResources . lensOf @Pdf.ResourcePath
            }
        educatorContext = E.EducatorContext
            { _ecResources = educatorResources
            , _ecWitnessVars = ctx ^. lensOf @W.WitnessVariables
            }
        newCfg = (finaliseDeferredUnsafe mempty) -- not great but it actually works
            & sub #core .~ multiEducatorConfig ^. sub #core
            & sub #witness .~ multiEducatorConfig ^. sub #witness
            & sub #educator . sub #db .~ multiEducatorConfig ^. sub #educator . sub #db
            & sub #educator . sub #keys . sub #keyParams .~ keyParams
            & sub #educator . sub #api . option #educatorAPINoAuth .~
                error "Do not touch, multi-educator has already run the server"
            & sub #educator . sub #publishing .~ multiEducatorConfig ^. sub #educator . sub #publishing

    workerAsyncs <- E.withEducatorConfig newCfg $ runRIO educatorContext $
        mapM (async . runWorker identity) $ educatorWorkers

    let loadedEducatorCtx = E.withEducatorConfig newCfg $ LoadedEducatorContext
            { lecCtx = educatorContext
            , lecWorkerHandlers = workerAsyncs
            }
    return loadedEducatorCtx

normalToMulti :: MultiEducatorWorkMode ctx m => LoadedEducatorContext -> E.EducatorRealMode a -> m a
normalToMulti LoadedEducatorContext{..} = runRIO lecCtx

----------------------------------------------------------------------------
-- Sanity check
----------------------------------------------------------------------------

_sanity :: MultiEducatorRealMode ()
_sanity = withMultiEducatorConfig (error "") $ W.withWitnessConfig (error "") _sanityCallee
  where
    _sanityCallee :: MultiCombinedWorkMode ctx m => m ()
    _sanityCallee = pass
