module Ariadne.Knit.Face where

import Control.Exception (SomeException)
import Data.List.NonEmpty (NonEmpty)
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Ariadne.TaskManager.Face
import qualified Knit

-- The result of executing a knit command.
data KnitCommandResult components
  = KnitCommandSuccess (Knit.Value components)
  | KnitCommandEvalError (Knit.EvalError components)
  | KnitCommandProcError (NonEmpty Knit.CommandId)
  | KnitCommandException SomeException

-- | Events as generated by the Knit interpreter. They will be translated into
-- UI-compatible events in the 'Glue' module. They must be independent from the
-- UI and capture /what the backend can generate/, not what the frontend can
-- handle.

data KnitCommandHandle components
  = KnitCommandHandle
  { putCommandResult :: (Maybe TaskId) -> KnitCommandResult components -> IO ()
  , putCommandOutput :: TaskId -> Doc -> IO ()
  }

-- API for the knit interpreter.
data KnitFace components =
  KnitFace
    {
      -- Execute a knit expression asynchronously. Does not block unless the
      -- queue of commands is full (should not normally happen) -- the result of
      -- execution will be returned later as an application event.
      putKnitCommand :: KnitCommandHandle components -> Knit.Expr Knit.CommandId components -> IO (Maybe TaskId)
      -- Get list of help items for each Knit command available
    , getKnitHelp :: Proxy components -> [Doc]
    }
