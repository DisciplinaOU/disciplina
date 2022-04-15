-- | Resources of every single educator.
module Dscp.MultiEducator.Launcher.Educator.Resource
    ( SingleEducatorResources (..)
    , educatorSchemaName
    , makeCertIssuerRes
    , educatorFromMultiEducatorConfig
    ) where

import Universum

import Fmt ((+|), (|+))
import Loot.Base.HasLens (HasCtx, lensOf)
import Loot.Log (LoggingIO, MonadLogging, logInfo)
import qualified Data.Text as T
import qualified Pdf.FromLatex as Pdf
import UnliftIO (MonadUnliftIO)

import Dscp.Config
import Dscp.Core.Foundation (Language, CertificateIssuerInfo (..), toItemDescUnsafe)
import Dscp.DB.SQL
import qualified Dscp.Educator.Config as E
import qualified Dscp.Educator.DB.Resource as E
import qualified Dscp.Educator.Launcher.Resource as E
import Dscp.MultiEducator.Config
import Dscp.MultiEducator.Launcher.Context
import Dscp.MultiEducator.Types
import Dscp.MultiEducator.Web.Educator.Auth
import Dscp.Resource.Class (AllocResource (..), buildComponentR)
import Dscp.Resource.AppDir

-- | Returns database schema name for an educator with given ID.
educatorSchemaName :: PubAddress -> Text
educatorSchemaName addr = "educator_" <> T.toLower (toText addr)

prepareMultiEducatorSchema
    :: ( MonadUnliftIO m, HasCtx ctx m '[LoggingIO], MonadLogging m
       , HasCallStack
       )
    => SQL -> PubAddress -> m ()
prepareMultiEducatorSchema db educatorAddr = do
    let schema = educatorSchemaName educatorAddr
    logInfo $ "Creating a schema with name `"+|schema|+"`"
    setSchemaName db schema
    E.prepareEducatorSchema db

instance AllocResource (PreparedSQL "multi-educator") where
    type Deps (PreparedSQL "multi-educator") = (PostgresRealParamsRec, PubAddress)
    allocResource p =
        PreparedSQL <$>
        buildComponentR "SQL DB" (openPostgresDB' p) closePostgresDB
      where
        openPostgresDB' (param, educatorAddr) = do
            db <- openPostgresDB (PostgresParams $ PostgresReal param)
            prepareMultiEducatorSchema db educatorAddr
            return db

-- | Context of some educator loaded by multi-educator.
newtype SingleEducatorResources = SingleEducatorResources E.EducatorResources

makeCertIssuerRes
    :: HasMultiEducatorConfig
    => EducatorAuthLogin
    -> E.CertificateIssuerResource
makeCertIssuerRes _educatorAuthLogin = E.KnownIssuerInfo $ CertificateIssuerInfo
  { ciiName    = toItemDescUnsafe "Disciplina Dev"
  , ciiWebsite = toItemDescUnsafe "http://disciplina.io"
  , ciiId      = "0xdA9a23c412CbBCdaB171a2218f3FeD7D0c0d9a9f"
  }
  -- TODO: make fetching the data from the token or service
  --   fetchPayload $ ealToken educatorAuthLogin
  -- where
  --   fetchPayload = undefined

educatorFromMultiEducatorConfig :: HasMultiEducatorConfig => E.EducatorConfigRec
educatorFromMultiEducatorConfig =
    (finaliseDeferredUnsafe mempty) -- not great but it actually works
        & sub #educator . sub #logging .~ multiEducatorConfig ^. sub #educator . sub #logging
        & sub #educator . sub #db .~ multiEducatorConfig ^. sub #educator . sub #db
        & sub #educator . sub #appDir .~ multiEducatorConfig ^. sub #educator . sub #appDir
        & sub #educator . option #pubAddress .~
            error "Do not touch, multi-educator has set up the public address"
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
        let educatorPubAddress = eadPublicAddress $ ealData educatorAuthLogin
            educatorConfig = multiEducatorConfig ^. sub #educator

        let _erLogging = multiEducatorResources ^. lensOf @LoggingIO
        let _erAppDir = multiEducatorResources ^. lensOf @AppDir
        let _erLanguage = multiEducatorResources ^. lensOf @Language
        let _erPdfLatexPath = multiEducatorResources ^. lensOf @Pdf.LatexPath
        let _erPdfResourcePath = multiEducatorResources ^. lensOf @Pdf.ResourcePath
        let _erDownloadBaseUrl = multiEducatorResources ^. lensOf @Pdf.DownloadBaseUrl
        let _erPdfCertIssuerRes = withMultiEducatorConfig multiEducatorConfig $
                                  makeCertIssuerRes educatorAuthLogin

        _erDB <-
            unPreparedSQL @"multi-educator" <$>
            allocResource (educatorConfig ^. sub #db, educatorPubAddress)
        _erPubAddress <- allocResource educatorPubAddress

        return (SingleEducatorResources E.EducatorResources{..})
