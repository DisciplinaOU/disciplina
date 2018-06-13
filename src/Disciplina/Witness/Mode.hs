{-# LANGUAGE TemplateHaskell #-}

-- | Module contains the definition of Witness's WorkMode and its implementations.

module Disciplina.Witness.Mode
    (
      -- * Constraints
      WitnessWorkMode

      -- * Implementations
    , WitnessContext (..)
    , WitnessRealMode
    , wcDB
    , wcLoggerName
    ) where

import Universum

import Control.Lens (makeLenses)
import Ether.Internal (HasLens (..))
import System.Wlog (HasLoggerName (..), LoggerName)

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

data WitnessContext = WitnessContext
    { _wcDB         :: NodeDB
    , _wcLoggerName :: LoggerName
    }

makeLenses ''WitnessContext

type WitnessRealMode = ReaderT WitnessContext IO

---------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------

instance HasLens NodeDB WitnessContext NodeDB where
    lensOf = wcDB

instance {-# OVERLAPPING #-} HasLoggerName WitnessRealMode where
    askLoggerName = view wcLoggerName
    modifyLoggerName name = local $ wcLoggerName %~ name

