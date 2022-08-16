-- | Context of multi-educator node.
module Dscp.MultiEducator.Launcher.Context
       ( EducatorContextVar
       , EducatorContextsMap
       , EducatorContexts (..)
       , EducatorContextsVar
       , MultiEducatorResources (..)
       , _ActiveEducatorContexts
       , merWitnessResources
       , merEducatorData
       ) where

import Control.Lens (makeLenses, makePrisms)
import qualified Pdf.FromLatex as Pdf

import Dscp.Core.Foundation (Language)
import Dscp.DB.SQL
import Dscp.MultiEducator.Launcher.Educator.Context
import Dscp.MultiEducator.Types
import Dscp.Resource.Network (NetServResources)
import Dscp.Util.HasLens
import qualified Dscp.Witness.Launcher.Resource as Witness

-- | We keep each educator's context in a separate @TVar@ to reduce
-- contention.
type EducatorContextVar = TVar MaybeLoadedEducatorContext

-- | For each educator - its context state.
type EducatorContextsMap = Map EducatorUUID EducatorContextVar

-- | State of active educator contexts.
data EducatorContexts
    = -- | Some contexts are in use.
      ActiveEducatorContexts EducatorContextsMap
      -- | Multi-educator is terminating and does not accept further
      -- contexts.
    | TerminatedEducatorContexts

makePrisms ''EducatorContexts

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
