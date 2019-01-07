#!/usr/bin/env stack
{- stack script
   --resolver ../../snapshot.yaml
   --install-ghc

   --package filepath
   --package lens
   --package loot-prelude
   --package shelly
-}

{-

===========================================================================
This script launches a single node, be it witness, educator or other one,
with predefined parameters.

-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

import "loot-prelude" Prelude hiding (FilePath)

import Control.Exception (AsyncException (..))
import Control.Lens (makeLenses, (.=))
import Shelly (FilePath, Sh, cmd, echo, errorExit, fromText, handle_sh, mkdir_p, quietExit, relPath,
               run, run_, setStdin, shelly, toTextWarn, which, writefile, (</>))
import System.FilePath.Posix (pathSeparator)

-- | '</>' for 'Text's.
(<//>) :: Text -> Text -> Text
path <//> item = path <> toText [pathSeparator] <> item

----------------------------------------------------------------------------
-- Parse commands
----------------------------------------------------------------------------

data RunNode
    = WitnessNode
    | EducatorNode
        { _nBot :: Bool
        }
    | MultiEducatorNode
        { _nBot :: Bool
        }
    | FaucetNode
    deriving (Show)
makeLenses ''RunNode

data Options = Options
    { _oNode    :: RunNode
      -- ^ Name of node to launch
    , _oClean   :: Bool
      -- ^ Do not clear temporal folders from the previous run
    , _oProfile :: Bool
      -- ^ Run with profiling
    } deriving (Show)
makeLenses ''Options

-- | Gathers options from CLI arguments.
-- Last options take a precedence.
parseArgs :: [String] -> Options
parseArgs args = execState (forM args parseArg) defCmd
  where
    defCmd = Options
        { _oNode = error "Please specify which node to run (e.g. \"educator\" or just \"e\")"
        , _oClean = True
        , _oProfile = False
        }
    parseArg arg
        | arg == "witness" || arg == "w" =
            oNode .= WitnessNode

        | arg == "educator" || arg == "e" =
            oNode .= EducatorNode{ _nBot = False }

        | arg == "multi-educator" || arg == "me" =
            oNode .= MultiEducatorNode{ _nBot = False }

        | arg == "bot" || arg == "b" =
            oNode . nBot .= True

        | arg == "faucet" || arg == "f" =
            oNode .= FaucetNode

        | arg == "--no-clean" =
            oClean .= False

        | arg == "--prof" || arg == "--profile" =
            oProfile .= True

        | otherwise = error $ "Unknown argument " <> show arg

----------------------------------------------------------------------------
-- Preliminaries
----------------------------------------------------------------------------

data Paths = Paths
    { filesDir    :: Text
    , tmpFilesDir :: Text
    }

getPaths :: RunNode -> Sh Paths
getPaths node = do
    filesDir <-
        toTextWarn =<< relPath "run"
    tmpFilesDir <-
        toTextWarn =<< relPath (fromText "run" </> fromText "tmp" </> fromText nodeTypeName)
    return Paths{..}
  where
    nodeTypeName = case node of
        WitnessNode{}       -> "witness"
        EducatorNode{}      -> "educator"
        MultiEducatorNode{} -> "multi-educator"
        FaucetNode{}        -> "faucet"

createPaths :: Paths -> Sh ()
createPaths Paths{..} =
    mkdir_p (fromText tmpFilesDir)

----------------------------------------------------------------------------
-- Parameters
----------------------------------------------------------------------------

witnessApiAddress :: Text
witnessApiAddress = "127.0.0.1:8091"

studentApiNoAuthAddress :: Text
studentApiNoAuthAddress = "3BAyX5pNpoFrLJcP5bZ2kXihBfmBVLprSyP1RhcPPddm6Dw42jzEPXZz22"

witnessParams :: Paths -> [Text]
witnessParams Paths{..} =
    [ "--appdir", "tmp"
    , "--config", "config.yaml"
    , "--config-key", "singleton"
    , "--bind", "127.0.0.1:4010:4011"
    , "--db-path", tmpFilesDir <//> "witness.db"
    , "--witness-listen", witnessApiAddress
    , "--comm-n", "0"
    , "--metrics-server", "127.0.0.1:8125"
    ]

educatorParams :: Paths -> [Text]
educatorParams paths@Paths{..} =
    witnessParams paths <>
    [ "--educator-keyfile", tmpFilesDir <//> "educator.key"
    , "--sql-path", tmpFilesDir <//> "educator.db"
    , "--educator-listen", "127.0.0.1:8090"
    , "--educator-api-no-auth"
    , "--student-api-no-auth", studentApiNoAuthAddress
    , "--publication-period", "15s"
    ]

multiEducatorParams :: Paths -> [Text]
multiEducatorParams = educatorParams

-- | Bot-only parameters.
educatorBotParams :: [Text]
educatorBotParams =
    [ "--educator-bot"
    , "--educator-bot-delay", "3s"
    ]

faucetParams :: Paths -> [Text]
faucetParams Paths{..} =
    [ "--appdir", "tmp"
    , "--faucet-listen", "127.0.0.1:8095"
    , "--witness-backend", witnessApiAddress
    , "--translated-amount", "20"
    , "--config", "config.yaml"
    , "--config-key", "singleton"
    , "--faucet-keyfile", filesDir <//> "faucet.key"
    , "--faucet-gen-key"
    ]

profilingParams :: [Text]
profilingParams = words "+RTS -p -RTS"

----------------------------------------------------------------------------
-- Launch
----------------------------------------------------------------------------

asText :: Text -> Text
asText = id

-- | Depending on disciplina source code is problematic, thus we call installed
-- "dscp-keygen" executable.
keygen :: Text -> Text -> Sh Text
keygen command seed = do
    installed <- isJust <$> which "dscp-keygen"
    unless installed $
        errorExit "dscp-keygen not found, install it with 'stack install disciplina-tools'"

    setStdin seed
    run "dscp-keygen" ["--seed", command]

genKeyfile :: FilePath -> Text -> Sh ()
genKeyfile keyfile seed = do
    secret <- keygen "keyfile" seed
    writefile keyfile secret
    cmd "chmod" (asText "0600") keyfile

execute :: Text -> [Text] -> Sh ()
execute name params = do
    let execPath = fromText name
    installed <- which execPath
    let doRun =
            if isJust installed
            then run_ execPath params
            else run_ "stack" ("exec" : name : "--" : params)
    ignoreInterruptedException doRun
  where
    -- Do not print operations log on user interrupt
    ignoreInterruptedException =
        handle_sh $ \case
            UserInterrupt -> echo "Interrupted" >> quietExit 1
            e -> throwM e

launch :: Options -> Sh ()
launch options = do
    paths <- getPaths (_oNode options)

    when (_oClean options) $
        cmd "rm" (asText "-rf") (tmpFilesDir paths)

    createPaths paths
    let commonParams = mconcat
            [ guard (_oProfile options) *> profilingParams
            ]

    case _oNode options of
        WitnessNode ->
            execute "dscp-witness" (witnessParams paths <> commonParams)

        EducatorNode botEnabled -> do
            let mBotParams = guard botEnabled *> educatorBotParams
            let keyfile = tmpFilesDir paths </> asText "educator.key"
            genKeyfile keyfile "educator-1"
            execute "dscp-educator" (educatorParams paths <> mBotParams <> commonParams)

        MultiEducatorNode botEnabled -> do
            let mBotParams = guard botEnabled *> educatorBotParams
            let keyfile = tmpFilesDir paths </> asText "educator.key"
            genKeyfile keyfile "multi-educator-1"
            execute "dscp-educator" (educatorParams paths <> mBotParams <> commonParams)

        FaucetNode ->
            execute "dscp-faucet" (faucetParams paths <> commonParams)

main :: IO ()
main = do
    options <- parseArgs <$> getArgs
    shelly $ launch options
