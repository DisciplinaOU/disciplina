module Dscp.System.Files
    ( getFirstExistingFile
    ) where

import System.Directory (doesFileExist)

-- | Return the first path in the given list which refers to an
-- existing file.
getFirstExistingFile :: MonadIO m => [FilePath] -> m (Maybe FilePath)
getFirstExistingFile = liftIO . fmap safeHead . filterM doesFileExist
