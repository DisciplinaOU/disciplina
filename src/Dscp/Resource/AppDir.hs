-- | Application folder resource.
-- For now we create it strictly on application startup,
-- which makes sense because logs go there anyway.

module Dscp.Resource.AppDir
    ( AppDirectoryParam (..)
    , AppDirectory (..)
    ) where

import Fmt ((+|), (|+))
import Loot.Log (MonadLogging, logInfo)
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, getXdgDirectory)

import Dscp.Resource.Class (AllocResource (..), buildComponentR)
import Dscp.System (appName)

-- | Which application directory to use.
data AppDirectoryParam
    = AppDirectoryOS  -- ^ Dedicated folder inside OS directory for applications

-- | If you hold this, you can assume that application dir exists.
newtype AppDirectory = AppDirectory { unAppDirectory :: FilePath }
    deriving (IsString)

-- | Return folder for this application, which will be within directory next to
-- other applications in the system, e.g. "~/.local/share/disciplina".
getOSAppDir :: MonadIO m => m FilePath
getOSAppDir = liftIO $ getXdgDirectory XdgData appName

-- | Create application directory if absent.
ensureDirExists
    :: (MonadIO m, MonadLogging m)
    => AppDirectoryParam -> m AppDirectory
ensureDirExists AppDirectoryOS = do
    appDir <- getOSAppDir
    logInfo $ "Application home directory will be at "+|appDir|+""
    liftIO $ createDirectoryIfMissing False appDir
    return $ AppDirectory appDir

instance AllocResource AppDirectoryParam AppDirectory where
    allocResource p =
        buildComponentR "Application directory"
            (ensureDirExists p)
            (\_ -> pass)
