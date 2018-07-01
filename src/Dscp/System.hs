-- | Interaction with OS.

module Dscp.System
    (
      -- * Env variables
      appDir

    , AppDirError (..)

      -- * Other
    , appName
    ) where

import qualified Data.Text.Buildable
import Fmt ((+|), (|+))
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Info (os)
import System.IO.Unsafe (unsafePerformIO)
import qualified Text.Show

---------------------------------------------------------------------
-- Environment
---------------------------------------------------------------------

-- | Defines an environmental variable.
-- Such approach is safe as soon as we do not modify environmental variables
-- within application.
env :: String -> Maybe String
env = unsafePerformIO . lookupEnv

-- | When failed to get OS-default application path.
data AppDirError
    = AppDirNoEnv String
    | AppDirUnknownOS String

instance Show AppDirError where
    show = toString . pretty

instance Buildable AppDirError where
    build = \case
        AppDirNoEnv key -> "Failed to get environmental variable "+|key|+""
        AppDirUnknownOS osname -> "Unknown OS "+|osname|+""

-- | Path to application dedicated directory.
appDir :: Either AppDirError FilePath
appDir = case os of
    "linux"   -> do
        home <- eenv "HOME"
        return $ home </> ".disciplina"
    "windows" -> do
        home <- eenv "APPDATA"
        return $ home </> "Disciplina"
    other -> Left $ AppDirUnknownOS other
  where
    eenv key = maybeToRight (AppDirNoEnv key) $ env key
{-# NOINLINE appDir #-}

---------------------------------------------------------------------
-- Other
---------------------------------------------------------------------

-- | Name of this application.
appName :: IsString s => s
appName = "disciplina"
{-# SPECIALIZE appName :: String #-}
{-# SPECIALIZE appName :: Text #-}

