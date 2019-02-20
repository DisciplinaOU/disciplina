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

import Control.Lens (makeLenses, (?~))
import qualified Data.Map as M
import Loot.Base.HasLens (HasLens', lensOf)
import Loot.Config (option, sub)
import Loot.Log (LoggingIO)
import qualified Pdf.FromLatex as Pdf
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix ((</>))

import Dscp.Config
import Dscp.Crypto
import Dscp.DB.SQL
import qualified Dscp.Educator.Config as E
import Dscp.Educator.DB (prepareEducatorSchema)
import Dscp.Educator.Launcher.Marker (EducatorNode)
import qualified Dscp.Educator.Launcher.Mode as E
import qualified Dscp.Educator.Launcher.Resource as E
import qualified Dscp.Launcher.Mode as Basic
import Dscp.MultiEducator.Config
import Dscp.MultiEducator.Launcher.Params (MultiEducatorKeyParams (..))
import Dscp.MultiEducator.Launcher.Resource (EducatorContexts (..), EducatorCtxWithCfg (..),
                                             MultiEducatorResources (..))
import Dscp.Resource.AppDir (getOSAppDir)
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
    ( Basic.BasicWorkMode m

    , HasWitnessConfig
    , HasMultiEducatorConfig

    , MonadReader ctx m

    , HasLens' ctx LoggingIO
    , HasLens' ctx (TVar EducatorContexts)

    -- It's easier to just have these two lenses instead of reconstructing
    -- the full educator context in multiToNormal from other lenses
    , HasLens' ctx MultiEducatorResources
    , HasLens' ctx W.WitnessVariables
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
lookupEducator :: MultiEducatorWorkMode ctx m => Text -> m EducatorCtxWithCfg
lookupEducator login = do
    mctx <- ask
    let MultiEducatorResources{..} = mctx ^. lensOf @MultiEducatorResources
    EducatorContexts ctxs <- atomically $ readTVar _merEducatorData
    maybe (loadEducator login Nothing) return $ M.lookup login ctxs
    -- TODO lookupEducator should probably take a Maybe PassPhrase
    -- or the relationship with loadEducator should be different/removed

loadEducator :: (MultiEducatorWorkMode ctx m) => Text -> Maybe PassPhrase -> m EducatorCtxWithCfg
loadEducator login mpassphrase = do
    -- TODO: add hashing
    let appDirParam = multiEducatorConfig ^. sub #witness . sub #appDir
    appDir <- case appDirParam ^. tree #param . selection of
        "os" -> getOSAppDir
        "specific" -> pure $
            appDirParam ^. tree #param . peekBranch #specific . option #path
        sel -> error $ "unknown AppDir type: " <> fromString sel
    ctx <- ask
    let resources = ctx ^. lensOf @MultiEducatorResources
        (MultiEducatorKeyParams keyFile) = multiEducatorConfig ^. sub #educator . option #keys
        db = resources ^. lensOf @SQL
    -- set the DB schema name and create it if it's not ready
    setSchemaName db ("educator_" <> login)
    prepareEducatorSchema db
    liftIO $ createDirectoryIfMissing True (appDir </> keyFile)
    -- read key from file and creates one if it does not exist yet
    let keyParams = finaliseDeferredUnsafe $ mempty
            & option #path       ?~ Just keyFile
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
            & sub #educator . sub #api .~ multiEducatorConfig ^. sub #educator . sub #api
            & sub #educator . sub #publishing .~ multiEducatorConfig ^. sub #educator . sub #publishing
        educatorCtxWithCfg = E.withEducatorConfig newCfg $ EducatorCtxWithCfg educatorContext
    -- add to/update the educator context map with the newly made one
    atomically $ modifyTVar' (_merEducatorData resources) $ \(EducatorContexts ctxs) ->
        EducatorContexts $ M.insert login educatorCtxWithCfg ctxs
    return educatorCtxWithCfg

normalToMulti :: MultiEducatorWorkMode ctx m => EducatorCtxWithCfg -> E.EducatorRealMode a -> m a
normalToMulti (EducatorCtxWithCfg ctx) = runRIO ctx

----------------------------------------------------------------------------
-- Sanity check
----------------------------------------------------------------------------

_sanity :: MultiEducatorRealMode ()
_sanity = withMultiEducatorConfig (error "") $ W.withWitnessConfig (error "") _sanityCallee
  where
    _sanityCallee :: MultiCombinedWorkMode ctx m => m ()
    _sanityCallee = pass
