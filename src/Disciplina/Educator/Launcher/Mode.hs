{-# LANGUAGE TemplateHaskell #-}

-- | Module contains the definition of Educator's WorkMode and its implementations.

module Disciplina.Educator.Launcher.Mode
    (
      -- * Constraints
      EducatorWorkMode
    , CombinedWorkMode

      -- * Implementations
    , EducatorContext (..)
    , EducatorRealMode
    ) where

import Universum

import Control.Lens (makeLenses)

import Disciplina.DB.Real.Types (NodeDB)
import qualified Disciplina.Launcher.Mode as Basic
import qualified Disciplina.Witness.Launcher as Witness
import Ether.Internal (HasLens (..))
import System.Wlog (HasLoggerName (..))

---------------------------------------------------------------------
-- WorkMode class
---------------------------------------------------------------------

-- | Set of typeclasses which define capabilities of bare Educator node.
type EducatorWorkMode m =
    ( Basic.BasicWorkMode m
    )

-- | Set of typeclasses which define capabilities both of Educator and Witness.
type CombinedWorkMode m =
    ( EducatorWorkMode m
    , Witness.WitnessWorkMode m
    )

---------------------------------------------------------------------
-- WorkMode implementation
---------------------------------------------------------------------

data EducatorContext = EducatorContext
    { _ecWitnessCtx :: Witness.WitnessContext
    }

makeLenses ''EducatorContext

type EducatorRealMode = ReaderT EducatorContext IO

---------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------

instance HasLens NodeDB EducatorContext NodeDB where
    lensOf = ecWitnessCtx . Witness.wcDB

instance {-# OVERLAPPING #-} HasLoggerName EducatorRealMode where
    askLoggerName = view (ecWitnessCtx . Witness.wcLoggerName)
    modifyLoggerName name = local $ (ecWitnessCtx . Witness.wcLoggerName) %~ name
