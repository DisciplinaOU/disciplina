{-# LANGUAGE OverloadedLabels #-}

-- | Resources used by Educator node

module Dscp.Educator.Launcher.Resource
       ( EducatorResources (..)
       , CertificateIssuerResource (..)
       , erWitnessResources
       , erDB
       , erKeys
       , erPdfLatexPath
       , erPdfResourcePath
       , erPdfCertIssuerRes
       ) where

import Control.Lens (makeLenses)
import Fmt ((+|), (|+))
import Loot.Log (logDebug)
import qualified Pdf.FromLatex as Pdf
import Servant.Client.Core.Internal.BaseUrl
import System.Directory (doesDirectoryExist, findExecutable)
import System.FilePath.Posix (isRelative, (</>))

import Dscp.Config
import Dscp.Core.Foundation.Educator (ItemDesc)
import Dscp.DB.SQL
import Dscp.Educator.Config
import Dscp.Educator.DB.Resource ()
import Dscp.Educator.Launcher.Marker (EducatorNode)
import Dscp.Educator.Launcher.Params
import Dscp.Resource.AppDir
import Dscp.Resource.Class (AllocResource (..), buildComponentR)
import Dscp.Resource.Keys (KeyResources (..), linkStore)
import Dscp.Resource.Network (NetServResources)
import Dscp.Util
import Dscp.Util.Exceptions
import Dscp.Util.HasLens
import qualified Dscp.Witness.Launcher.Resource as Witness

-- | Educator Resource that either has the info 'CertificateIssuerInfo' itself
-- or enough info to contact a server that has it
data CertificateIssuerResource
    = KnownIssuerInfo Pdf.CertificateIssuerInfo
    | FromServiceIssuerInfo BaseUrl ByteString

-- | Datatype which contains resources required by all Disciplina nodes
-- to start working.
data EducatorResources = EducatorResources
    { _erWitnessResources :: !Witness.WitnessResources
    , _erDB               :: !SQL
    , _erKeys             :: !(KeyResources EducatorNode)
    , _erPdfLatexPath     :: !Pdf.LatexPath
    , _erPdfResourcePath  :: !Pdf.ResourcePath
    , _erDownloadBaseUrl  :: !Pdf.DownloadBaseUrl
    , _erPdfCertIssuerRes :: !CertificateIssuerResource
    }

makeLenses ''EducatorResources
deriveHasLensDirect ''EducatorResources

deriveHasLens 'erWitnessResources ''EducatorResources ''Witness.WitnessResources
deriveHasLens 'erWitnessResources ''EducatorResources ''NetServResources

instance AllocResource (KeyResources EducatorNode) where
    type Deps (KeyResources EducatorNode) = (EducatorKeyParamsRec, AppDir)
    allocResource (educatorCfg, appDir) =
        let baseParams = educatorCfg ^. sub #keyParams
        in buildComponentR "educator keys"
           (linkStore baseParams appDir)
           (const pass)

instance AllocResource Pdf.LatexPath where
    type Deps Pdf.LatexPath = FilePath
    allocResource executable =
        buildComponentR "LaTeX generator executable" checkExec (const pass)
      where
        checkExec = do
            logDebug $ "Checking for LaTeX executable at "+|executable|+""
            mPath <- liftIO $ findExecutable executable
            Pdf.LatexPath <$>
                nothingToThrow (ExecutableNotFound "LaTeX generator" executable)
                mPath

instance AllocResource Pdf.ResourcePath where
    type Deps Pdf.ResourcePath = (FilePath, AppDir)
    allocResource (userPath, AppDir appDir) =
        buildComponentR "LaTeX resources path" preparePath (\_ -> pass)
      where
        resPath
            | isRelative userPath = appDir </> userPath
            | otherwise = userPath
        preparePath = do
            logDebug $ "Certificate PDF resources path will be " +| resPath |+ ""
            unlessM (liftIO $ doesDirectoryExist resPath) $
                throwM $ DirectoryDoesNotExist "pdf templates" resPath
            return $ Pdf.ResourcePath resPath

instance AllocResource Pdf.DownloadBaseUrl where
    type Deps Pdf.DownloadBaseUrl = BaseUrl
    allocResource = return . Pdf.DownloadBaseUrl

instance AllocResource CertificateIssuerResource where
    type Deps CertificateIssuerResource = (ItemDesc, ItemDesc, Text)
    allocResource (ciiName, ciiWebsite, ciiId) =
        return . KnownIssuerInfo $ Pdf.CertificateIssuerInfo {..}

instance AllocResource EducatorResources where
    type Deps EducatorResources = EducatorConfigRec
    allocResource educatorCfg = do
        let cfg = educatorCfg ^. sub #educator
            witnessCfg = rcast educatorCfg
        _erWitnessResources <- withWitnessConfig witnessCfg $
                               allocResource witnessCfg
        _erDB <- unPreparedSQL @"educator" <$> allocResource (cfg ^. sub #db)
        let appDir = Witness._wrAppDir _erWitnessResources
        _erKeys <- allocResource (educatorCfg ^. sub #educator . sub #keys, appDir)
        _erPdfLatexPath <- allocResource $ cfg ^. sub #certificates . option #latex
        _erPdfResourcePath <- allocResource ( cfg ^. sub #certificates . option #resources
                                            , appDir )
        _erDownloadBaseUrl <- allocResource $ cfg ^. sub #certificates . option #downloadBaseUrl
        _erPdfCertIssuerRes <- allocResource
            ( cfg ^. sub #certificates . sub #issuer . option #name
            , cfg ^. sub #certificates . sub #issuer . option #website
            , cfg ^. sub #certificates . sub #issuer . option #id
            )
        return EducatorResources {..}
