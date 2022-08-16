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
    ) where

import Control.Lens (makeLenses)
import Control.Monad.Fix (MonadFix)
import Loot.Base.HasLens (HasLens')
import Loot.Network.Class (NetworkingCli, NetworkingServ)
import Loot.Network.ZMQ (ZmqTcp)

import Dscp.DB.SQL
import Dscp.Educator.Launcher.Marker (EducatorNode)
import Dscp.MultiEducator.Config
import Dscp.MultiEducator.Launcher.Context
import Dscp.Resource.Network
import Dscp.Rio (RIO)
import Dscp.Util.HasLens
import qualified Dscp.Witness as W

---------------------------------------------------------------------
-- WorkMode class
---------------------------------------------------------------------

-- | Set of typeclasses which define capabilities of bare Educator node.
type MultiEducatorWorkMode ctx m =
    ( W.WitnessWorkMode ctx m
    , MonadFix m

    , HasMultiEducatorConfig
    , HasLens' ctx EducatorContextsVar
    , HasLens' ctx SQL

    -- It's easier to just have these two lenses instead of reconstructing
    -- the full educator context in multiToNormal from other lenses
    , HasLens' ctx MultiEducatorResources
    , HasLens' ctx W.WitnessVariables

    , NetworkingCli ZmqTcp m
    , NetworkingServ ZmqTcp m
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
      -- ^ Runtime variables of witness.
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
-- Sanity check
----------------------------------------------------------------------------

_sanity :: MultiEducatorRealMode ()
_sanity = withMultiEducatorConfig (error "") $ W.withWitnessConfig (error "") _sanityCallee
  where
    _sanityCallee :: MultiCombinedWorkMode ctx m => m ()
    _sanityCallee = pass
