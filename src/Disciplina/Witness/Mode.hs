{-# LANGUAGE TemplateHaskell #-}

-- | Module contains the definition of Witness's WorkMode and its implementations.

module Disciplina.Witness.Mode
    (
      -- * Constraints
      WitnessWorkMode

      -- * Implementations
    , Witness
    , WitnessCustomContext (..)
    , WitnessRealMode

      -- * Re-exports
    , Basic.RealMode
    ) where

import Universum

import Control.Lens (makeLenses)
import Ether.Internal (HasLens (..))

import Disciplina.DB.Class (MonadDB)
import Disciplina.DB.Real.Types (NodeDB)
import qualified Disciplina.Launcher.Mode as Basic

---------------------------------------------------------------------
-- WorkMode class
---------------------------------------------------------------------

-- | Set of typeclasses which define capabilities of Witness node.
type WitnessWorkMode m =
    ( Basic.BasicWorkMode m
    , MonadDB m
    )

---------------------------------------------------------------------
-- WorkMode implementation
---------------------------------------------------------------------

-- | Witness node role.
data Witness

type instance Basic.CustomContext Witness = WitnessCustomContext
data WitnessCustomContext = WitnessCustomContext
    { _wccDB :: NodeDB
    }

makeLenses ''WitnessCustomContext

type WitnessRealMode = Basic.RealMode Witness

---------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------

instance HasLens NodeDB (Basic.NodeContext Witness) NodeDB where
    lensOf = Basic.ncCustomCtx . wccDB
