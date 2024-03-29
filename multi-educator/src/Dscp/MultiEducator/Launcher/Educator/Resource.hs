-- | Resources of every single educator.
module Dscp.MultiEducator.Launcher.Educator.Resource
    ( SingleEducatorResources (..)
    , educatorSchemaName
    , makeCertIssuerRes
    , educatorFromMultiEducatorConfig
    ) where

import Control.Lens (zoom, (.=))
import Fmt ((+|), (|+))
import Loot.Base.HasLens (HasCtx, lensOf)
import Loot.Log (LoggingIO, MonadLogging, logInfo)
import qualified Pdf.FromLatex as Pdf
import System.FilePath ((<.>), (</>))
import UnliftIO (MonadUnliftIO)

import Dscp.Config
import Dscp.Core.Foundation (Language)
import Dscp.Crypto
import Dscp.DB.SQL
import qualified Dscp.Educator.Config as E
import qualified Dscp.Educator.DB.Resource as E
import qualified Dscp.Educator.Launcher.Params as E
import qualified Dscp.Educator.Launcher.Resource as E
import Dscp.MultiEducator.Config
import Dscp.MultiEducator.Launcher.Context
import Dscp.MultiEducator.Launcher.Params
import Dscp.MultiEducator.Types
import Dscp.MultiEducator.Web.Educator.Auth
import Dscp.Resource.Class (AllocResource (..), buildComponentR)
import Dscp.Util
import Dscp.Web.Server
import qualified Dscp.Witness.Launcher.Resource as W

-- | Returns database schema name for an educator with given ID.
educatorSchemaName :: EducatorUUID -> Text
educatorSchemaName (EducatorUUID eId) = "educator_" <> eId

prepareMultiEducatorSchema
    :: ( MonadUnliftIO m, HasCtx ctx m '[LoggingIO], MonadLogging m
       , HasCallStack
       )
    => SQL -> EducatorUUID -> m ()
prepareMultiEducatorSchema db educatorId = do
    let schema = educatorSchemaName educatorId
    logInfo $ "Creating a schema with name `"+|schema|+"`"
    setSchemaName db schema
    E.prepareEducatorSchema db

instance AllocResource (PreparedSQL "multi-educator") where
    type Deps (PreparedSQL "multi-educator") = (PostgresRealParamsRec, EducatorUUID)
    allocResource p =
        PreparedSQL <$>
        buildComponentR "SQL DB" (openPostgresDB' p) closePostgresDB
      where
        openPostgresDB' (param, educatorId) = do
            db <- openPostgresDB (PostgresParams $ PostgresReal param)
            prepareMultiEducatorSchema db educatorId
            return db

-- | Context of some educator loaded by multi-educator.
newtype SingleEducatorResources = SingleEducatorResources E.EducatorResources

mkSingleEducatorKeyParams
    :: MultiEducatorKeyParams
    -> EducatorUUID
    -> Maybe PassPhrase
    -> E.EducatorKeyParamsRec
mkSingleEducatorKeyParams
    (MultiEducatorKeyParams keyDir)
    (EducatorUUID educatorId)
    mpassphrase =
        finaliseDeferredUnsafe mempty &: do
            zoom (sub #keyParams) $ do
                option #path       .= Just (keyDir </> toString educatorId <.> "key")
                option #genNew     .= True
                option #passphrase .= mpassphrase

makeCertIssuerRes
    :: HasMultiEducatorConfig
    => EducatorAuthLogin
    -> E.CertificateIssuerResource
makeCertIssuerRes educatorAuthLogin =
    let aaaUrl = multiEducatorConfig ^. sub #educator . sub #aaa . option #serviceUrl
    in E.FromServiceIssuerInfo
        (aaaUrl { baseUrlPath = "educators" </> "current"})
        (ealToken educatorAuthLogin)

educatorFromMultiEducatorConfig :: HasMultiEducatorConfig => E.EducatorConfigRec
educatorFromMultiEducatorConfig =
    (finaliseDeferredUnsafe mempty) -- not great but it actually works
        & sub #core .~ multiEducatorConfig ^. sub #core
        & sub #witness .~ multiEducatorConfig ^. sub #witness
        & sub #educator . sub #db .~ multiEducatorConfig ^. sub #educator . sub #db
        & sub #educator . sub #keys . sub #keyParams .~
            error "Do not touch, multi-educator has allocated all keys"
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
  where
    multiEducatorSub = multiEducatorConfig ^. sub #educator

instance HasMultiEducatorConfig => AllocResource SingleEducatorResources where
    type Deps SingleEducatorResources =
        (MultiEducatorResources, EducatorAuthLogin)

    allocResource (multiEducatorResources, educatorAuthLogin) = do
        let educatorId = eadId $ ealData educatorAuthLogin
        let mpassphrase = Nothing

        let educatorConfig = multiEducatorConfig ^. sub #educator
            witnessResources = _merWitnessResources multiEducatorResources
            appDir = W._wrAppDir witnessResources
            meKeyParams = educatorConfig ^. option #keys

        let _erWitnessResources = witnessResources
        let _erLanguage = multiEducatorResources ^. lensOf @Language
        let _erPdfLatexPath = multiEducatorResources ^. lensOf @Pdf.LatexPath
        let _erPdfResourcePath = multiEducatorResources ^. lensOf @Pdf.ResourcePath
        let _erDownloadBaseUrl = multiEducatorResources ^. lensOf @Pdf.DownloadBaseUrl
        let _erPdfCertIssuerRes = withMultiEducatorConfig multiEducatorConfig $
                                  makeCertIssuerRes educatorAuthLogin

        _erDB <-
            unPreparedSQL @"multi-educator" <$>
            allocResource (educatorConfig ^. sub #db, educatorId)
        _erKeys <-
            allocResource
                ( mkSingleEducatorKeyParams meKeyParams educatorId mpassphrase
                , appDir
                )

        return (SingleEducatorResources E.EducatorResources{..})
