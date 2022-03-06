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
    ) where

import Universum

import Control.Lens (makeLenses)
import Loot.Base.HasLens (HasLens', HasCtx)
import Loot.Log (LoggingIO)
import qualified Pdf.FromLatex as Pdf

import Dscp.Core (Language)
import Dscp.DB.SQL
import Dscp.Educator.Launcher.Marker (EducatorNode)
import Dscp.Educator.Launcher.Mode (BasicWorkMode)
import Dscp.MultiEducator.Config
import Dscp.MultiEducator.Launcher.Context
import Dscp.Resource.AppDir
import Dscp.Rio (RIO)
import Dscp.Util.HasLens

---------------------------------------------------------------------
-- WorkMode class
---------------------------------------------------------------------

-- | Set of typeclasses which define capabilities of bare Educator node.
type MultiEducatorWorkMode ctx m =
    ( BasicWorkMode m
    , HasMultiEducatorConfig
    , HasLens' ctx MultiEducatorResources
    , HasCtx ctx m
        [ LoggingIO
        , AppDir
        , SQL
        , EducatorContextsVar
        , Language
        , Pdf.LatexPath
        , Pdf.ResourcePath
        , Pdf.DownloadBaseUrl
        ]
    )

-- | Set of typeclasses which define capabilities both of Educator and Witness^W NO WITNESSES ANYMORE
type MultiCombinedWorkMode ctx m = MultiEducatorWorkMode ctx m

---------------------------------------------------------------------
-- WorkMode implementation
---------------------------------------------------------------------

data MultiEducatorContext = MultiEducatorContext
    { _mecResources   :: !MultiEducatorResources
      -- ^ Resources, allocated from params.
    }

makeLenses ''MultiEducatorContext
deriveHasLensDirect ''MultiEducatorContext

type MultiEducatorRealMode = RIO MultiEducatorContext

---------------------------------------------------------------------
-- HasLens
---------------------------------------------------------------------

deriveHasLens 'mecResources ''MultiEducatorContext ''MultiEducatorResources

----------------------------------------------------------------------------
-- Sanity check
----------------------------------------------------------------------------

_sanity :: MultiEducatorRealMode ()
_sanity = withMultiEducatorConfig (error "") _sanityCallee
  where
    _sanityCallee :: MultiCombinedWorkMode ctx m => m ()
    _sanityCallee = pass
