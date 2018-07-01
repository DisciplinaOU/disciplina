-- | Application folder resource.
-- For now we create it strictly on application startup,
-- which makes sense because logs go there anyway.

module Dscp.Resource.AppDir
    ( AppDirectoryParam (..)
    , AppDirectory (..)
    , AppDirectoryCreationError (..)
    ) where

import Control.Monad.Component (buildComponent)
import qualified Data.Text.Buildable
import Fmt ((+|), (|+))
import System.Directory (createDirectoryIfMissing)
import qualified Text.Show

import Dscp.Resource.Class (AllocResource (..))
import Dscp.System (AppDirError (..), appDir)
import Dscp.Util (leftToThrow)

-- | Which application directory to use.
data AppDirectoryParam
    = AppDirectoryAtHome  -- ^ Dedicated folder inside home dir

-- | If you hold this, you can assume that application dir exists.
newtype AppDirectory = AppDirectory { unAppDirectory :: FilePath }
    deriving (IsString)

-- | App directory allocation error.
data AppDirectoryCreationError
    = AppDirectoryRecognitionError AppDirError

instance Buildable AppDirectoryCreationError where
    build = \case
        AppDirectoryRecognitionError err ->
            "Failed to discover app directory: "+|err|+""

instance Show AppDirectoryCreationError where
    show = toString . pretty

instance Exception AppDirectoryCreationError

-- | Create application directory if absent.
ensureDirExists
    :: (MonadIO m, MonadThrow m)
    => AppDirectoryParam -> m AppDirectory
ensureDirExists AppDirectoryAtHome = do
    app <- leftToThrow AppDirectoryRecognitionError appDir
    liftIO $ createDirectoryIfMissing False app
    return $ AppDirectory app

instance AllocResource AppDirectoryParam AppDirectory where
    allocResource p =
        buildComponent "Application directory"
            (ensureDirExists p)
            (\_ -> pass)
