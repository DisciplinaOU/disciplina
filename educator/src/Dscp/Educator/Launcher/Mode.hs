{-# LANGUAGE TemplateHaskell #-}

-- | Module contains the definition of Educator's WorkMode and its implementations.

module Dscp.Educator.Launcher.Mode
    (
      -- * Markers
      EducatorNode

      -- * Constraints
    , BasicWorkMode
    , EducatorWorkMode
    , FullEducatorWorkMode

      -- * Implementations
    , EducatorContext (..)
    , ecResources
    , EducatorRealMode
    ) where

import Universum

import Control.Lens (makeLenses)
import Loot.Base.HasLens (HasCtx)
import Loot.Log (LoggingIO, WithLogging)
import qualified Pdf.FromLatex as Pdf
import UnliftIO (MonadUnliftIO)

import Dscp.Core (Language, PubAddress)
import Dscp.DB.SQL (SQL)
import Dscp.Educator.Config (HasEducatorConfig, withEducatorConfig)
import Dscp.Educator.Launcher.Marker (EducatorNode)
import Dscp.Educator.Launcher.Resource (CertificateIssuerResource, EducatorResources)
import Dscp.Resource.AppDir
import Dscp.Rio (RIO)
import Dscp.Util.HasLens

-- | Set of typeclasses which define basic capabilities of Disciplina node
type BasicWorkMode m =
    ( WithLogging m
    , MonadIO m
    , MonadUnliftIO m  -- allows to use lifted-async
    , MonadMask m
    )

---------------------------------------------------------------------
-- WorkMode class
---------------------------------------------------------------------

-- | Set of typeclasses which define capabilities of bare Educator node.
type EducatorOnlyWorkMode ctx m =
    ( BasicWorkMode m
    , HasEducatorConfig
    , HasCtx ctx m
        [ LoggingIO
        , AppDir
        , SQL
        , PubAddress
        , Language
        , Pdf.LatexPath
        , Pdf.ResourcePath
        , Pdf.DownloadBaseUrl
        , CertificateIssuerResource
        ]
    )

-- | Set of typeclasses which define capabilities both of Educator and Witness.
-- type EducatorWorkMode ctx m =
--     ( EducatorOnlyWorkMode ctx m
--     , W.WitnessWorkMode ctx m
--     )

-- type FullEducatorWorkMode ctx m =
--     ( EducatorOnlyWorkMode ctx m
--     , W.FullWitnessWorkMode ctx m
--     )

type EducatorWorkMode ctx m = EducatorOnlyWorkMode ctx m
type FullEducatorWorkMode ctx m = EducatorOnlyWorkMode ctx m

---------------------------------------------------------------------
-- WorkMode implementation
---------------------------------------------------------------------

data EducatorContext = EducatorContext
    { _ecResources   :: !EducatorResources
      -- ^ Resources, allocated from params.
    }

makeLenses ''EducatorContext
deriveHasLensDirect ''EducatorContext

type EducatorRealMode = RIO EducatorContext

---------------------------------------------------------------------
-- HasLens
---------------------------------------------------------------------

deriveHasLens 'ecResources ''EducatorContext ''EducatorResources

----------------------------------------------------------------------------
-- Sanity check
----------------------------------------------------------------------------

_sanity :: EducatorRealMode ()
_sanity = withEducatorConfig (error "") _sanityCallee
  where
    _sanityCallee :: EducatorWorkMode ctx m => m ()
    _sanityCallee = pass
