-- | File modes management.

module Dscp.System.FileMode
    ( getAccessMode
    ) where

import Fmt (octF, (+|), (|+))
import Loot.Log (MonadLogging, logWarning)
import qualified System.Posix.Files as PSX
import qualified System.Posix.Types as PSX (FileMode)

import Dscp.System.Other (IsPosix)

-- | Return only the access part of the file mode (like owner:rw-, etc).
getAccessMode :: (IsPosix, MonadIO m) => FilePath -> m PSX.FileMode
getAccessMode path = do
    mode <- liftIO $ PSX.fileMode <$> PSX.getFileStatus path
    return $ PSX.intersectFileModes mode PSX.accessModes
