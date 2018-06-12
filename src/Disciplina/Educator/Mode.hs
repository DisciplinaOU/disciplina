{-# LANGUAGE TemplateHaskell #-}

-- | Module contains the definition of Educator's WorkMode and its implementations.

module Disciplina.Educator.Mode
    (
      -- * Constraints
      EducatorWorkMode

      -- * Implementations
    , Educator
    , EducatorCustomContext (..)
    , EducatorRealMode

      -- * Re-exports
    , Basic.RealMode
    ) where

import Control.Lens (makeLenses)

import qualified Disciplina.Launcher.Mode as Basic

---------------------------------------------------------------------
-- WorkMode class
---------------------------------------------------------------------

-- | Set of typeclasses which define capabilities of Educator node.
type EducatorWorkMode m =
    ( Basic.BasicWorkMode m
    )

---------------------------------------------------------------------
-- WorkMode implementation
---------------------------------------------------------------------

-- | Educator node role.
data Educator

type instance Basic.CustomContext Educator = EducatorCustomContext
data EducatorCustomContext = EducatorCustomContext

makeLenses ''EducatorCustomContext

type EducatorRealMode = Basic.RealMode Educator

