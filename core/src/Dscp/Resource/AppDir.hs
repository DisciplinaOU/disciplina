{-# LANGUAGE OverloadedLabels #-}

-- | Application folder which we carry in config.
-- For now we create it strictly on application startup,
-- which makes sense because logs go there anyway.

module Dscp.Resource.AppDir
       ( AppDirParam
       , AppDirParamRec
       , AppDirParamRecP

       , AppDir (..)
       , getOSAppDir
       ) where

import Universum
import Fmt ((+|), (|+))
import Loot.Config ((::+), (::-), (:::), Config, PartialConfig)
import Loot.Log (MonadLogging, logInfo)
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, getXdgDirectory)
import System.Environment (lookupEnv)
import System.IO.Error (catchIOError, ioError, isDoesNotExistError)

import Dscp.Config
import Dscp.Resource.Class (AllocResource (..), buildComponentR)
import Dscp.System (appName)

-- | Which application directory to use.
-- Note, there is a reason this contains the whole tree and not just its content
-- see 'ConfigMaybe' for an explanation.
type AppDirParam =
   '[ "param" ::+
       '[ "os" ::- '[]
          -- Dedicated folder inside OS directory for applications
        , "specific" ::- '[ "path" ::: FilePath ]
          -- Given path
        ]
    ]

type AppDirParamRec = Config AppDirParam
type AppDirParamRecP = PartialConfig AppDirParam


newtype AppDir = AppDir FilePath

-- | Return folder for this application, which will be within directory next to
-- other applications in the system, e.g. "~/.local/share/disciplina".
getOSAppDir :: MonadIO m => m FilePath
getOSAppDir = liftIO $
    getXdgDirectory XdgData appName `catchIOError` \e ->
        -- only for `DoesNotExistError`s, this is to provide a fallback in case
        -- `$HOME` is not set
        if isDoesNotExistError e
        then fromMaybe "." <$> lookupEnv "PWD"
        else ioError e

-- | Create application directory if absent.
prepareAppDir
    :: (MonadIO m, MonadLogging m)
    => AppDirParamRec -> m AppDir
prepareAppDir dirConfig = do
    appDir <- case dirConfig ^. tree #param . selection of
        "os"       -> getOSAppDir
        "specific" -> pure $
            dirConfig ^. tree #param . peekBranch #specific . option #path
        sel -> error $ "unknown AppDir type: " <> fromString sel
    -- we would unlikely have logging context here
    logInfo $ "Application home directory will be at "+|appDir|+""
    liftIO $ createDirectoryIfMissing True appDir
    return $ AppDir appDir

instance AllocResource AppDir where
    type Deps AppDir = AppDirParamRec
    allocResource p = buildComponentR "AppDir" (prepareAppDir p) (\_ -> pass)
