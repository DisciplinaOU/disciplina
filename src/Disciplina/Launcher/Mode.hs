{-# LANGUAGE TemplateHaskell #-}

-- | Module contains the definition of WorkMode and its implementations.

module Disciplina.Launcher.Mode
       (
         -- * Constraints
         BasicWorkMode

         -- * Implementations
       , BasicRealMode
       , BasicRealContext (..)
       ) where

import Universum

import Control.Lens (makeLenses)
import Mockable (MonadMockable, Production (..))
import System.Wlog (HasLoggerName (..), LoggerName, WithLogger)

import Disciplina.DB.Class (MonadDB)

---------------------------------------------------------------------
-- WorkMode classes
---------------------------------------------------------------------

-- | Set of typeclasses which define basic capabilities of Disciplina node
type BasicWorkMode m =
    ( WithLogger m
    , MonadMockable m
    , MonadIO m
    , MonadDB m
    )

---------------------------------------------------------------------
-- WorkMode implementations
---------------------------------------------------------------------

data BasicRealContext = BasicRealContext
    { _brcLoggerName :: LoggerName
    }

makeLenses ''BasicRealContext

type BasicRealMode = ReaderT BasicRealContext Production

instance {-# OVERLAPPING #-} HasLoggerName BasicRealMode where
    askLoggerName = view brcLoggerName
    modifyLoggerName name = local $ brcLoggerName %~ name
