
-- | Helpers for starting a Disciplina node

module Disciplina.Launcher.Runner
       ( runRealMode
       , prepareAndRunRealMode
       ) where

import Universum

import Control.Monad.Component (runComponentM)

import Disciplina.Launcher.Mode (NodeContext (..))
import qualified Disciplina.Launcher.Mode as Mode
import Disciplina.Launcher.Resource (AllocResource (..))

-- | Given allocated node resources, construct node context and run `WorkMode` monad.
runRealMode
    :: NodeContext r
    -> Mode.RealMode r a
    -> IO a
runRealMode ctx action = runReaderT action ctx

-- | Given params, allocate resources, construct node context and run `WorkMode` monad.
prepareAndRunRealMode
    :: AllocResource params resources
    => (resources -> NodeContext r) -> params -> Mode.RealMode r a -> IO a
prepareAndRunRealMode formCtx params action =
    runComponentM "Disciplina (real mode)" (allocResource params) $
      \resources ->
        let ctx = formCtx resources
        in runRealMode ctx action
