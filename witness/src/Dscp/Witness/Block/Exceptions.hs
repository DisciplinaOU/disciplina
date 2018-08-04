-- | Block handling exceptions.

module Dscp.Witness.Block.Exceptions
    ( BlockLogicException (..)
    ) where

import Control.Exception (Exception)

data BlockLogicException
    = BEBlockAbsent Text
    | BEMalformed Text
    deriving (Eq, Show)

instance Exception BlockLogicException
