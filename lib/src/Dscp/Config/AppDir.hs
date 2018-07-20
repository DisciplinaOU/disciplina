-- | Application folder which we carry in config.
-- For now we create it strictly on application startup,
-- which makes sense because logs go there anyway.

module Dscp.Config.AppDir
    ( AppDirectoryParam (..)
    , prepareAppDir
    ) where

import qualified Data.Yaml as Y
import Fmt ((+|), (|+))
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, getXdgDirectory)

import Dscp.System (appName)

-- | Which application directory to use.
data AppDirectoryParam
    = AppDirectoryOS
      -- ^ Dedicated folder inside OS directory for applications
    | AppDirectorySpecific !FilePath
      -- ^ Given path

instance Y.FromJSON AppDirectoryParam where
    parseJSON = asum . sequence
        [ \v -> do
            Y.String "os_app_dir" <- pure v
            return AppDirectoryOS
        , Y.withObject "Specific app path" $ \o ->
            AppDirectorySpecific <$> (o Y..: "path")
        ]

-- | Return folder for this application, which will be within directory next to
-- other applications in the system, e.g. "~/.local/share/disciplina".
getOSAppDir :: MonadIO m => m FilePath
getOSAppDir = liftIO $ getXdgDirectory XdgData appName

-- | Create application directory if absent.
prepareAppDir
    :: MonadIO m
    => AppDirectoryParam -> m FilePath
prepareAppDir param = do
    appDir <- case param of
        AppDirectoryOS            -> getOSAppDir
        AppDirectorySpecific path -> pure path
    -- we would unlikely have logging context here
    putTextLn $ "Application home directory will be at "+|appDir|+""
    liftIO $ createDirectoryIfMissing True appDir
    return appDir
