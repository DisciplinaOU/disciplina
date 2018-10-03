-- | Application folder which we carry in config.
-- For now we create it strictly on application startup,
-- which makes sense because logs go there anyway.

module Dscp.Resource.AppDir
       ( AppDirParam(..)
       , AppDir
       ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Fmt ((+|), (|+))
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, getXdgDirectory)

import Dscp.Resource.Class (AllocResource (..), buildComponentR)
import Dscp.System (appName)

-- | Which application directory to use.
data AppDirParam
    = AppDirectoryOS
      -- ^ Dedicated folder inside OS directory for applications
    | AppDirectorySpecific !FilePath
      -- ^ Given path
    deriving (Show, Eq)

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

instance AllocResource AppDir where
    type Deps AppDir = AppDirParam
    allocResource p = buildComponentR "AppDir" (prepareAppDir p) (\_ -> pass)

-- | Isomorphism between @Maybe FilePath@ and 'AppDirParam'
maybeToAppDirParam :: Maybe FilePath -> AppDirParam
maybeToAppDirParam Nothing   = AppDirectoryOS
maybeToAppDirParam (Just fp) = AppDirectorySpecific fp

appDirParamToMaybe :: AppDirParam -> Maybe FilePath
appDirParamToMaybe AppDirectoryOS            = Nothing
appDirParamToMaybe (AppDirectorySpecific fp) = Just fp

-- | JSON instances for 'AppDirParam'
instance FromJSON AppDirParam where
    parseJSON = fmap maybeToAppDirParam . parseJSON
instance ToJSON AppDirParam where
    toJSON = toJSON . appDirParamToMaybe
