module Dscp.Util.FileEmbed
    ( embedSomeStringFile
    , embedSubprojectStringFile
    ) where

import Data.FileEmbed (embedStringFile)
import Dscp.System.Files (getFirstExistingFile)
import qualified Language.Haskell.TH as TH
import System.FilePath.Posix ((</>))

-- | Like 'embedStringFile', but tries multiple paths and embeds the first
-- referring to an actual file.
embedSomeStringFile :: [FilePath] -> TH.Q TH.Exp
embedSomeStringFile paths = do
    mpath <- TH.runIO $ getFirstExistingFile paths
    path <- whenNothing mpath $
        fail $ "All paths refer to non-existing file: " <> (show paths)
    embedStringFile path

-- | Safely embed a file assuming that you are in a root of stack
-- subproject with the given name.
--
-- Use of plain 'embedStringFile' works well until you build in the same
-- subproject where the file is located, but building dependent subproject
-- with ghci/intero will break since working directory will change.
embedSubprojectStringFile :: String -> FilePath -> TH.Q TH.Exp
embedSubprojectStringFile subproject relPath =
    embedSomeStringFile $
        map (</> relPath) [".", subproject, ".." </> subproject]
