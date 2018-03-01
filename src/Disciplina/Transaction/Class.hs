
module Disciplina.Transaction.Class where

import Control.Monad.Catch

import Disciplina.WorldState

class MonadThrow m => IsTransaction t w m | t -> w m where
    play :: t -> w -> m w
