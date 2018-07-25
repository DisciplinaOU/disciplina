{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes   #-}

-- | Common (among witness and educator) CLI params.

module Dscp.CommonCLI
       ( logParamsParser
       , versionOption
       , keyParamsParser
       ) where

import Data.Version (showVersion)
import qualified Loot.Log as Log
import Options.Applicative (Parser, help, infoOption, long, metavar, option, str, strOption, switch)
import Text.InterpolatedString.Perl6 (qc)

import Dscp.Crypto (mkPassPhrase)
import Dscp.Resource.Keys (KeyParams (..))
import Dscp.Resource.Logging (LoggingParams (..))
import Dscp.Util (leftToFail)
import Paths_disciplina_witness (version)

logParamsParser :: Log.Name -> Parser LoggingParams
logParamsParser lpDefaultName = do
    lpDebug <- logDebugParser
    lpConfigPath <- logConfigParser
    lpDirectory <- logDirParser
    return LoggingParams {..}
  where
    logDebugParser = switch $
        long "debug" <>
        help "Switch default logging level from Info to Debug"
    logConfigParser = optional $ strOption $
        long "log-config" <>
        metavar "FILEPATH" <>
        help "Path to logger configuration."
    logDirParser = optional $ strOption $
        long "log-dir" <>
        metavar "FILEPATH" <>
        help "Path to logs directory."

versionOption :: Parser (a -> a)
versionOption = infoOption ("disciplina-" <> (showVersion version)) $
    long "version" <>
    help "Show version."

keyParamsParser :: Text -> Parser KeyParams
keyParamsParser who = do
    kpPath <- kpKeyPathParser
    kpGenNew <- kpGenKeyParser
    kpPassphrase <- kpPassphraseParser
    pure KeyParams{..}
  where
    kpKeyPathParser = optional . strOption $
         long [qc|{who}-keyfile-path|] <>
         metavar "FILEPATH" <>
         help [qc|Path to the secret key of {who}.|]
    kpGenKeyParser = switch $
         long [qc|{who}-generate-new-key|] <>
         help [qc|Generate the key and write it to '{who}-keyfile-path' path.|]
    kpPassphraseParser = optional . option passphraseReadM $
         long [qc|{who}-keyfile-password|] <>
         metavar "PASSWORD" <>
         help "Password of secret key."
    passphraseReadM = leftToFail . first pretty . mkPassPhrase =<< str
