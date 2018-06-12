
-- | Helpers for starting a Disciplina node

module Disciplina.Launcher.Runner
       ( runRealMode
       , prepareAndRunRealMode
       ) where

import Universum

import Disciplina.Launcher.Mode (FormNodeContext (..), NodeContext (..))
import qualified Disciplina.Launcher.Mode as Mode
import Disciplina.Launcher.Resource (BracketResource (..))

-- | Given allocated node resources, construct node context and run `WorkMode` monad.
runRealMode
    :: (FormNodeContext resources ctx, ctx ~ NodeContext r)
    => resources
    -> Mode.RealMode r a
    -> IO a
runRealMode res action =
    formNodeContext res >>= runReaderT action

-- | Given params, allocate resources, construct node context and run `WorkMode` monad.
prepareAndRunRealMode
    :: ( BracketResource params resources
       , FormNodeContext resources ctx
       , ctx ~ NodeContext r
       )
    => params -> Mode.RealMode r a -> IO a
prepareAndRunRealMode params action =
    bracketResource params (\res -> runRealMode res action)
