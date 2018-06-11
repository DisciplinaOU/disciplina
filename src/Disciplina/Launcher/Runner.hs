
-- | Helpers for starting a Disciplina node

module Disciplina.Launcher.Runner
       ( runBasicRealMode
       , runEducatorRealMode
       , runWitnessRealMode
       ) where

import Universum

import Control.Monad.Reader (withReaderT)
import Disciplina.Launcher.Mode (NodeContext (..), ncCustomCtx)
import qualified Disciplina.Launcher.Mode as Mode
import Disciplina.Launcher.Resource (BasicNodeResources (..))

-- | Given allocated node resources, construct node context and run `WorkMode` monad.
runBasicRealMode ::
       BasicNodeResources
    -> Mode.RealMode Mode.Basic a
    -> IO a
runBasicRealMode BasicNodeResources {..} action =
    runReaderT action context
  where
    context = NodeContext
        { _ncLoggerName = bnrLoggerName
        , _ncCustomCtx = Mode.NoCustomContext
        }

runEducatorRealMode ::
      BasicNodeResources
   -> Mode.RealMode Mode.Educator a
   -> IO a
runEducatorRealMode bres@BasicNodeResources{..} action = do
    let educatorCtx = Mode.EducatorCustomContext
    runBasicRealMode bres $ withReaderT (ncCustomCtx .~ educatorCtx) action

runWitnessRealMode ::
      BasicNodeResources
   -> Mode.RealMode Mode.Witness a
   -> IO a
runWitnessRealMode bres@BasicNodeResources{..} action = do
    let witnessCtx = Mode.WitnessCustomContext
            { _wcDB = bnrDB
            }
    runBasicRealMode bres $ withReaderT (ncCustomCtx .~ witnessCtx) action
