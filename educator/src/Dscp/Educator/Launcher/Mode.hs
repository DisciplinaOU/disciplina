{-# LANGUAGE TemplateHaskell #-}

-- | Module contains the definition of Educator's WorkMode and its implementations.

module Dscp.Educator.Launcher.Mode
    (
      -- * Markers
      EducatorNode

      -- * Constraints
    , EducatorWorkMode
    , FullEducatorWorkMode

      -- * Implementations
    , EducatorContext (..)
    , ecResources
    , ecWitnessVars
    , EducatorRealMode
    ) where

import Control.Lens (makeLenses)
import Loot.Base.HasLens (HasCtx)
import qualified Pdf.FromLatex as Pdf

import Dscp.DB.CanProvideDB as DB
import Dscp.DB.SQL (SQL)
import Dscp.Educator.Config (HasEducatorConfig, withEducatorConfig)
import Dscp.Educator.Launcher.Marker (EducatorNode)
import Dscp.Educator.Launcher.Resource (CertificateIssuerResource, EducatorResources)
import qualified Dscp.Launcher.Mode as Basic
import Dscp.Resource.Keys (KeyResources)
import Dscp.Resource.Network
import Dscp.Rio (RIO)
import Dscp.Util.HasLens
import qualified Dscp.Witness as W

---------------------------------------------------------------------
-- WorkMode class
---------------------------------------------------------------------

-- | Set of typeclasses which define capabilities of bare Educator node.
type EducatorOnlyWorkMode ctx m =
    ( Basic.BasicWorkMode m

    , HasEducatorConfig

    , HasCtx ctx m
        [ DB.Plugin
        , SQL
        , KeyResources EducatorNode
        , Pdf.LatexPath
        , Pdf.ResourcePath
        , Pdf.DownloadBaseUrl
        , CertificateIssuerResource
        ]
    )

-- | Set of typeclasses which define capabilities both of Educator and Witness.
type EducatorWorkMode ctx m =
    ( EducatorOnlyWorkMode ctx m
    , W.WitnessWorkMode ctx m
    )

type FullEducatorWorkMode ctx m =
    ( EducatorOnlyWorkMode ctx m
    , W.FullWitnessWorkMode ctx m
    )

---------------------------------------------------------------------
-- WorkMode implementation
---------------------------------------------------------------------

data EducatorContext = EducatorContext
    { _ecResources   :: !EducatorResources
      -- ^ Resources, allocated from params.
    , _ecWitnessVars :: !W.WitnessVariables
      -- ^ Witness variables (non-resources).
    }

makeLenses ''EducatorContext
deriveHasLensDirect ''EducatorContext

type EducatorRealMode = RIO EducatorContext

---------------------------------------------------------------------
-- HasLens
---------------------------------------------------------------------

deriveHasLens 'ecResources ''EducatorContext ''EducatorResources
deriveHasLens 'ecResources ''EducatorContext ''W.WitnessResources
deriveHasLens 'ecResources ''EducatorContext ''NetServResources
deriveHasLens 'ecWitnessVars ''EducatorContext ''W.WitnessVariables

----------------------------------------------------------------------------
-- Sanity check
----------------------------------------------------------------------------

_sanity :: EducatorRealMode ()
_sanity = withEducatorConfig (error "") $ W.withWitnessConfig (error "") _sanityCallee
  where
    _sanityCallee :: EducatorWorkMode ctx m => m ()
    _sanityCallee = pass
