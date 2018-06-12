
-- | Helpers for starting an Educator node

module Disciplina.Educator.Launcher.Runner where

import Universum

import Disciplina.Educator.Launcher.Params (EducatorParams (..))
import Disciplina.Educator.Launcher.Resource (EducatorResources (..))
import Disciplina.Educator.Mode (Educator, EducatorCustomContext (..), RealMode)
import Disciplina.Launcher.Mode (NodeContext (..))
import Disciplina.Launcher.Resource (BasicNodeResources (..))
import Disciplina.Launcher.Runner (prepareAndRunRealMode)

-- | Make up Educator context from dedicated pack of allocated resources.
formEducatorContext :: EducatorResources -> NodeContext Educator
formEducatorContext EducatorResources{..} =
    NodeContext
    { _ncLoggerName = bnrLoggerName erBasicResources
    , _ncCustomCtx = EducatorCustomContext
    }

-- | Given params, allocate resources, construct node context and run
-- `EducatorWorkMode` monad.
launchEducatorRealMode :: EducatorParams -> RealMode Educator a -> IO a
launchEducatorRealMode = prepareAndRunRealMode formEducatorContext
