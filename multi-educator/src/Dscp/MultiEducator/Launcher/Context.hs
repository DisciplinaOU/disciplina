-- | Context of multi-educator node.
module Dscp.MultiEducator.Launcher.Context
       ( EducatorContexts
       , EducatorContextsVar
       , MultiEducatorResources (..)
       , merWitnessResources
       , merEducatorData
       ) where

import Control.Lens (makeLenses)
import qualified Pdf.FromLatex as Pdf

import Dscp.Core.Foundation (Language)
import Dscp.DB.SQL
import Dscp.MultiEducator.Launcher.Educator.Context
import Dscp.MultiEducator.Types
import Dscp.Resource.Network (NetServResources)
import Dscp.Util.HasLens
import qualified Dscp.Witness.Launcher.Resource as Witness

-- | State of active educator contexts.
type EducatorContexts = Map EducatorUUID MaybeLoadedEducatorContext

-- | Contexts of every loaded educator.
type EducatorContextsVar = TVar EducatorContexts

-- | Datatype which contains resources required by all Disciplina nodes
-- to start working.
data MultiEducatorResources = MultiEducatorResources
    { _merWitnessResources :: !Witness.WitnessResources
    , _merDB               :: !SQL
    , _merEducatorData     :: !EducatorContextsVar
    , _merLanguage         :: !Language
    , _merPdfLatexPath     :: !Pdf.LatexPath
    , _merPdfResourcePath  :: !Pdf.ResourcePath
    , _merDownloadBaseUrl  :: !Pdf.DownloadBaseUrl
    }

makeLenses ''MultiEducatorResources
deriveHasLensDirect ''MultiEducatorResources

deriveHasLens 'merWitnessResources ''MultiEducatorResources ''Witness.WitnessResources
deriveHasLens 'merWitnessResources ''MultiEducatorResources ''NetServResources
