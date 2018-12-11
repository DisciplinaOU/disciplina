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

import Control.Lens (makeLenses)
import qualified Data.Map as M
import Loot.Base.HasLens (HasLens, lensOf)
import Loot.Config (option, sub)
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.FilePath.Posix ((</>))

import Dscp.Config
import Dscp.Crypto (mkPassPhrase)
import Dscp.DB.SQLite
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
import Dscp.Resource.AppDir (AppDirParam (..))
import Dscp.Resource.Keys (BaseKeyParams (..), linkStore)
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

    , HasLens ctx (TVar EducatorContexts)

    -- It's easier to just have these two (or a bit more) lenses instead of reconstructing
    -- the full educator context in multiToNormal from other lenses
    , HasLens ctx MultiEducatorResources
    , HasLens ctx W.WitnessVariables
    , HasLens ctx W.WitnessResources
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
lookupEducator :: MultiEducatorWorkMode ctx m => Text -> m (Maybe EducatorCtxWithCfg)
lookupEducator login = do
    mctx <- ask
    let MultiEducatorResources{ _merEducatorData } = mctx ^. lensOf
    fmap (\(EducatorContexts ctxs) -> M.lookup login ctxs) . atomically $ readTVar _merEducatorData

loadEducator :: MultiEducatorWorkMode ctx m => Bool -> Text -> Text -> m Bool
loadEducator _new login passphrase = do
    -- TODO: add hashing
    let loginFile = id (toString login)
    let appDir = case multiEducatorConfig ^. sub #witness . option #appDir of
          AppDirectoryOS         -> ""
          AppDirectorySpecific x -> x
    let (MultiEducatorKeyParams path) = multiEducatorConfig ^. sub #educator . option #keys
        prepareDb = do
            p <- case multiEducatorConfig ^. sub #educator . option #db of
                SQLiteParams (SQLiteReal (SQLiteRealParams fp con pen)) -> do
                    dbsPath <- canonicalizePath $ appDir </> fp
                    createDirectoryIfMissing True $ dbsPath
                    print dbsPath
                    pure $ SQLiteParams (SQLiteReal (SQLiteRealParams (dbsPath </> loginFile) con pen))
                x -> pure x
            db <- openSQLiteDB p
            prepareEducatorSchema db
            return (db, p)
    (db, _dbParam) <- liftIO $ prepareDb
    liftIO $ createDirectoryIfMissing True (appDir </> path)
    let keyFile = path </> loginFile <> ".key"
        -- FIXME
        (Right p) = mkPassPhrase . encodeUtf8 $ passphrase
    let keyParams = BaseKeyParams (Just keyFile) True (Just p)
    key <- withCoreConfig (rcast multiEducatorConfig) $ linkStore keyParams appDir
    -- FIXME: DB is not closed
    ctx <- ask
    let educatorResources = E.EducatorResources
            { _erWitnessResources = ctx ^. lensOf
            , _erKeys = key
            , _erDB = db
            }
        educatorContext = E.EducatorContext
            { _ecResources = educatorResources
            , _ecWitnessVars = ctx ^. lensOf
            }
        --newCfg' = E.defaultEducatorConfig
        --    & (sub #educator . option #db ?~ dbParam)
        --    . (sub #educator . option #keys ?~ E.EducatorKeyParams keyParams)
        newCfg = error "no config"
    atomically $ modifyTVar' (ctx ^. lensOf @_ @(TVar EducatorContexts))
        $ \(EducatorContexts ctxs) -> EducatorContexts $
            case M.lookup login ctxs of
                Just _ -> ctxs
                Nothing -> M.insert login (E.withEducatorConfig newCfg $ EducatorCtxWithCfg educatorContext) ctxs
    return True

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
