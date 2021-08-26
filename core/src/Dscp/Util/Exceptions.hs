module Dscp.Util.Exceptions
    ( FileSysException (..)
    ) where

import Fmt (Buildable (..), (+|), (|+))
import qualified Text.Show

data FileSysException
    = DirectoryDoesNotExist Text FilePath
    | ExecutableNotFound Text FilePath

instance Exception FileSysException

instance Show FileSysException where
    show = toString . pretty

instance Buildable FileSysException where
    build (DirectoryDoesNotExist desc dir) =
        "Failed to find "+|desc|+" directory: "+|dir|+""
    build (ExecutableNotFound desc path) =
        "Failed to find "+|desc|+" executable: "+|path|+""
