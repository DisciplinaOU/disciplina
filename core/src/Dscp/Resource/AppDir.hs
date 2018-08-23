-- | Application folder which we carry in config.
-- For now we create it strictly on application startup,
-- which makes sense because logs go there anyway.

module Dscp.Resource.AppDir
       ( AppDirParam(..)
       , AppDir
       ) where

import Fmt ((+|), (|+))
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, getXdgDirectory)

import Dscp.System (appName)
import Dscp.Resource.Class (AllocResource (..), buildComponentR)

-- | Which application directory to use.
data AppDirParam
    = AppDirectoryOS
      -- ^ Dedicated folder inside OS directory for applications
    | AppDirectorySpecific !FilePath
      -- ^ Given path
    deriving (Show)

type AppDir = FilePath

-- | Return folder for this application, which will be within directory next to
-- other applications in the system, e.g. "~/.local/share/disciplina".
getOSAppDir :: MonadIO m => m FilePath
getOSAppDir = liftIO $ getXdgDirectory XdgData appName

-- | Create application directory if absent.
prepareAppDir
    :: MonadIO m
    => AppDirParam -> m AppDir
prepareAppDir param = do
    appDir <- case param of
        AppDirectoryOS            -> getOSAppDir
        AppDirectorySpecific path -> pure path
    -- we would unlikely have logging context here
    putTextLn $ "Application home directory will be at "+|appDir|+""
    liftIO $ createDirectoryIfMissing True appDir
    return appDir

instance AllocResource AppDirParam AppDir where
    allocResource p = buildComponentR "AppDir" (prepareAppDir p) (\_ -> pass)
