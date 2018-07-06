-- | File modes management.

module Dscp.System.FileMode
    ( getAccessMode
    , setMode
    , ensureModeIs

    , mode600
    ) where

import Fmt (octF, (+|), (|+))
import Loot.Log (MonadLogging, logWarning)
import qualified System.Posix.Files as PSX
import qualified System.Posix.Types as PSX (FileMode)

import Dscp.System.Other (IsPosix)

-- | It's common practice to make keyfiles readable/writable for user only.
-- For now keyfile is read-only
mode600 :: PSX.FileMode
mode600 = PSX.unionFileModes PSX.ownerReadMode PSX.ownerWriteMode

-- | Return only the access part of the file mode (like owner:rw-, etc).
getAccessMode :: (IsPosix, MonadIO m) => FilePath -> m PSX.FileMode
getAccessMode path = do
    mode <- liftIO $ PSX.fileMode <$> PSX.getFileStatus path
    return $ PSX.intersectFileModes mode PSX.accessModes

-- | Set mode disregard current mode of the file.
setMode :: (IsPosix, MonadIO m) => PSX.FileMode -> FilePath -> m ()
setMode mode path = liftIO $ PSX.setFileMode path mode

-- | Set given mode if needed.
ensureModeIs
    :: (IsPosix, MonadIO m, MonadLogging m)
    => PSX.FileMode -> FilePath -> m ()
ensureModeIs mode path = do
    accessMode <- getAccessMode path
    unless (accessMode == mode) $ do
        logWarning $
            "Key file at "+|path|+" has access mode "+|octF accessMode|+
            " instead of 600. Fixing it automatically."
        setMode mode path
