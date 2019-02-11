module Dscp.Util.Exceptions
    ( DirectoryDoesNotExist (..)
    ) where

import Fmt (Buildable (..), (+|), (|+))
import qualified Text.Show

data DirectoryDoesNotExist = DirectoryDoesNotExist Text FilePath
instance Exception DirectoryDoesNotExist

instance Show DirectoryDoesNotExist where
    show = toString . pretty

instance Buildable DirectoryDoesNotExist where
    build (DirectoryDoesNotExist desc dir) =
        "Failed to find " +| desc |+ " directory: " +| dir |+ ""
