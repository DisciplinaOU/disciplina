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
    , educatorSchemaName
    , normalToMulti
    ) where

import Control.Concurrent.STM (retry)
import Control.Lens (at, makeLenses, traversed, zoom, (?~))
import Fmt ((+|), (|+))
import Loot.Base.HasLens (HasLens', lensOf)
import Loot.Config (option, sub)
import Loot.Log (logDebug, logInfo)
import Loot.Network.Class (NetworkingCli, NetworkingServ)
import Loot.Network.ZMQ (ZmqTcp)
import qualified Pdf.FromLatex as Pdf
import Serokell.Util (modifyTVarS)
import Servant.Client.Core.Internal.BaseUrl
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
import Dscp.Educator.Launcher.Resource (CertificateIssuerResource (..))
import qualified Dscp.Educator.Launcher.Resource as E
import Dscp.Educator.Workers
import Dscp.MultiEducator.Config
import Dscp.MultiEducator.Launcher.Params (MultiEducatorKeyParams (..))
import Dscp.MultiEducator.Launcher.Resource
import Dscp.MultiEducator.Web.Educator.Auth (EducatorAuthData (..), EducatorAuthLogin (..))
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
    , HasLens' ctx EducatorContextsVar
    , HasLens' ctx SQL

    -- It's easier to just have these two lenses instead of reconstructing
    -- the full educator context in multiToNormal from other lenses
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
-- It create a new Educator context if login was not found
lookupEducator
    :: MultiEducatorWorkMode ctx m
    => EducatorAuthLogin
    -> m LoadedEducatorContext
lookupEducator educatorAuthLogin = do
    EducatorContextsVar educatorContexts <-
        view $ lensOf @MultiEducatorResources . merEducatorData

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
                    lift retry

                Just (FullyLoadedEducatorContext ctx) ->
                    -- already prepared
                    return (Just ctx)

        case mctx of
            Nothing -> let rollback = withEducatorContextAtomically $ put Nothing
                in (`onException` rollback) $ do
                        ctx <- loadEducator educatorAuthLogin Nothing
                        withEducatorContextAtomically $
                            put $ Just (FullyLoadedEducatorContext ctx)
                        return ctx
            -- Note: we need this because even if we already have the 'EducatorCtxWithCfg'
            -- in the map it may need to be updated with a new token
            Just ctx -> do
                certIssuerInfoRes <- makeCertIssuerRes educatorAuthLogin
                let newEdCtx = lecCtx ctx & E.ecResources.E.erPdfCertIssuerRes .~ certIssuerInfoRes
                    newCtxWithCfg = ctx {lecCtx = newEdCtx}
                withEducatorContextAtomically $
                    put $ Just (FullyLoadedEducatorContext newCtxWithCfg)
                return newCtxWithCfg
    -- TODO lookupEducator should probably take a Maybe PassPhrase
    -- to pass it to 'loadEducator'

loadEducator
    :: (MultiEducatorWorkMode ctx m)
    => EducatorAuthLogin
    -> Maybe PassPhrase
    -> m LoadedEducatorContext
loadEducator educatorAuthLogin mpassphrase = do
    let educatorId = eadId $ ealData educatorAuthLogin
    logDebug $ "Loading educator " +| educatorId |+ ""
    -- TODO: add hashing
    appDir <- view $ lensOf @AppDir
    ctx <- ask
    let multiEducatorSub = multiEducatorConfig ^. sub #educator
        resources = ctx ^. lensOf @MultiEducatorResources
        (MultiEducatorKeyParams keyDir) = multiEducatorConfig ^. sub #educator . option #keys
        dbParams = multiEducatorConfig ^. sub #educator . sub #db

    -- open the new DB connection
    db <- openPostgresDB (PostgresParams $ PostgresReal dbParams)
    -- set the DB schema name and create it if it's not ready
    let schema = educatorSchemaName educatorId
    logInfo $ "Creating a schema with name `"+|schema|+"`"
    setSchemaName db schema
    prepareEducatorSchema db
    liftIO $ createDirectoryIfMissing True keyDir
    -- read key from file and creates one if it does not exist yet
    let keyParams = finaliseDeferredUnsafe $ mempty
            & option #path       ?~ Just (keyDir </> toString educatorId <.> "key")
            & option #genNew     ?~ True
            & option #passphrase ?~ mpassphrase
    key <- withCoreConfig (rcast multiEducatorConfig) $ linkStore keyParams appDir
    -- build up a new educator context with config
    certIssuerInfoRes <- makeCertIssuerRes educatorAuthLogin
    let educatorResources = E.EducatorResources
            { _erWitnessResources = resources ^. lensOf @W.WitnessResources
            , _erKeys = key
            , _erDB = db
            , _erPdfLatexPath = ctx ^. lensOf @MultiEducatorResources . lensOf @Pdf.LatexPath
            , _erPdfResourcePath = ctx ^. lensOf @MultiEducatorResources . lensOf @Pdf.ResourcePath
            , _erPdfCertIssuerRes = certIssuerInfoRes
            }
        educatorContext = E.EducatorContext
            { _ecResources = educatorResources
            , _ecWitnessVars = ctx ^. lensOf @W.WitnessVariables
            }
        newCfg = (finaliseDeferredUnsafe mempty) -- not great but it actually works
            & sub #core .~ multiEducatorConfig ^. sub #core
            & sub #witness .~ multiEducatorConfig ^. sub #witness
            & sub #educator . sub #db .~ dbParams
            & sub #educator . sub #keys . sub #keyParams .~ keyParams
            & sub #educator . sub #api . option #educatorAPINoAuth .~
                error "Do not touch, multi-educator has already run the server"
            & sub #educator . sub #publishing .~ multiEducatorSub ^. sub #publishing
            & sub #educator . sub #certificates . option #latex .~
                multiEducatorSub ^. sub #certificates . option #latex
            & sub #educator . sub #certificates . option #resources .~
                multiEducatorSub ^. sub #certificates . option #resources
            & sub #educator . sub #certificates . sub #issuer . option #name .~
                error "Should not use certificate config, resource is made by multi-educator"
            & sub #educator . sub #certificates . sub #issuer . option #website .~
                error "Should not use certificate config, resource is made by multi-educator"

    workerAsyncs <- E.withEducatorConfig newCfg $ runRIO educatorContext $
        let meWorkers = educatorWorkers
                      & traversed . wIdL %~ (<> "_of_" <> encodeUtf8 educatorId)
        in mapM (async . runWorker identity) meWorkers

    let loadedEducatorCtx = E.withEducatorConfig newCfg $ LoadedEducatorContext
            { lecCtx = educatorContext
            , lecWorkerHandlers = workerAsyncs
            }
    return loadedEducatorCtx

-- | Returns database schema name for an educator with given ID.
educatorSchemaName :: Text -> Text
educatorSchemaName eId = "educator_" <> eId

makeCertIssuerRes
    :: MultiEducatorWorkMode ctx m
    => EducatorAuthLogin
    -> m CertificateIssuerResource
makeCertIssuerRes educatorAuthLogin = do
    let aaaUrl = multiEducatorConfig ^. sub #educator . sub #aaa . option #serviceUrl
    return $ FromServiceIssuerInfo
        (aaaUrl { baseUrlPath = "educators" </> "current"})
        (ealToken educatorAuthLogin)

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
