-- | Logic-related exceptions.

module Dscp.Witness.Logic.Exceptions
    ( LogicException (..)
    ) where

import Control.Exception (Exception)

data LogicException
    = LEBlockAbsent Text
    | LEMalformed Text
    deriving (Eq, Show)

instance Exception LogicException
