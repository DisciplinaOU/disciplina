-- | Context of multi-educator node.
module Dscp.MultiEducator.Launcher.Context
       ( EducatorContexts
       , EducatorContextsVar
       , MultiEducatorResources (..)
       , merEducatorData
       ) where


import Universum

import Control.Lens (makeLenses)
import Loot.Log.Rio (LoggingIO)
import qualified Pdf.FromLatex as Pdf

import Dscp.Core.Foundation (Language)
import Dscp.DB.SQL
import Dscp.MultiEducator.Launcher.Educator.Context
import Dscp.MultiEducator.Types
import Dscp.Resource.AppDir
import Dscp.Util.HasLens

-- | State of active educator contexts.
type EducatorContexts = Map PubAddress MaybeLoadedEducatorContext

-- | Contexts of every loaded educator.
type EducatorContextsVar = TVar EducatorContexts

-- | Datatype which contains resources required by all Disciplina nodes
-- to start working.
data MultiEducatorResources = MultiEducatorResources
    { _merLogging         :: !LoggingIO
    , _merAppDir          :: !AppDir
    , _merDB              :: !SQL
    , _merEducatorData    :: !EducatorContextsVar
    , _merLanguage        :: !Language
    , _merPdfLatexPath    :: !Pdf.LatexPath
    , _merPdfResourcePath :: !Pdf.ResourcePath
    , _merDownloadBaseUrl :: !Pdf.DownloadBaseUrl
    }

makeLenses ''MultiEducatorResources
deriveHasLensDirect ''MultiEducatorResources
