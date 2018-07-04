-- | File modes management.

module Dscp.System.FileMode
    ( getAccessMode
    , setMode
    , ensureModeIs

    , mode600
    ) where

import qualified System.Posix.Files as PSX
import qualified System.Posix.Types as PSX (FileMode)

import Dscp.System.Other (IsPosix)

-- | It's common practice to make keyfiles readable/writable for user only.
-- For now keyfile is read-only
mode600 :: PSX.FileMode
mode600 = PSX.unionFileModes PSX.ownerReadMode PSX.ownerWriteMode

-- Note: these are 'IO' for purpose and that means that you may want to rethrow
-- I/O exceptions as something more sensible at call site.
-- Consider using 'Dscp.Util.wrapRethrowIO' instead of 'liftIO'.

-- | Return only the access part of the file mode (like owner:rw-, etc).
getAccessMode :: IsPosix => FilePath -> IO PSX.FileMode
getAccessMode path = do
    mode <- liftIO $ PSX.fileMode <$> PSX.getFileStatus path
    return $ PSX.intersectFileModes mode PSX.accessModes

-- | Set mode disregard current mode of the file.
setMode :: IsPosix => FilePath -> PSX.FileMode -> IO ()
setMode = PSX.setFileMode

-- | Set given mode if needed.
ensureModeIs :: IsPosix => PSX.FileMode -> FilePath -> IO ()
ensureModeIs mode path = do
    accessMode <- getAccessMode path
    unless (accessMode == mode) $ do
        -- TODO [DSCP-122]: uncomment
        -- logWarning $
        --     sformat ("Key file at "%build%" has access mode "%oct%" instead of 600. Fixing it automatically.")
        --     path accessMode
        setMode path mode600
