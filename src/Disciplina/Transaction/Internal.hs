
module Disciplina.Transaction.Internal where

import Universum

import Control.Monad.Catch

import Disciplina.Transaction.Class
import Disciplina.WorldState

data NullTransaction w (m :: * -> *) = NullTransaction

instance MonadThrow m => IsTransaction (NullTransaction w m) w m where
    play NullTransaction ws = pure ws
