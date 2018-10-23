{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes   #-}

-- | Common CLI params.

module Dscp.CommonCLI
       ( logParamsParser
       , versionOption
       , baseKeyParamsParser
       , passphraseReadM
       , timeReadM
       , coinReadM
       , addressReadM
       , networkAddressParser
       , clientAddressParser
       , serverParamsParser
       , appDirParamParser
       ) where

import Data.Char (toLower)
import Data.Version (showVersion)
import qualified Loot.Log as Log
import Options.Applicative (Parser, ReadM, auto, eitherReader, flag', help, infoOption, long,
                            maybeReader, metavar, option, str, strOption, switch)
import Servant.Client (BaseUrl (..), parseBaseUrl)
import Text.InterpolatedString.Perl6 (qc)
import Time (KnownRatName, Time, unitsP)

import Dscp.Core.Foundation
import Dscp.Crypto (PassPhrase)
import Dscp.Crypto (mkPassPhrase)
import Dscp.Resource.AppDir
import Dscp.Resource.Keys (BaseKeyParams (..))
import Dscp.Resource.Logging (LoggingParams (..))
import Dscp.Util
import Dscp.Web (NetworkAddress (..), ServerParams (..), parseNetAddr)
import Paths_disciplina_witness (version)

{-
[Note default-cli-params]

Several parsers in this file are changed in order to not
yield default values when no related CLI arguments are provided.
Parsers yielding default values always override corresponding values
in config file, making using config files without CLI params at all
effectively impossible.

To support default values of configuration parameters nevertheless,
default config values are provided in respective `*.Config` modules.
-}

logParamsParser :: Log.Name -> Parser LoggingParams
logParamsParser lpDefaultName = do
    lpLoggingType <- logTypeParser
    lpDebug <- logDebugParser
    -- [Note default-cli-params]
    lpConfigPath <- Just <$> logConfigParser
    lpDirectory <- logDirParser
    return LoggingParams {..}
  where
    logTypeParser = optional $ option auto $
        long "log-type" <>
        metavar "[Syslog | Warper]" <>
        help "Logging type to use (Syslog or Warper). If not specified, \
             \Syslog will be used."
    logDebugParser = switch $
        long "debug" <>
        help "Switch default logging level from Info to Debug"
    logConfigParser = strOption $
        long "log-config" <>
        metavar "FILEPATH" <>
        help "Path to logger configuration. If not specified, some default config is used."
    logDirParser = optional $ strOption $
        long "log-dir" <>
        metavar "FILEPATH" <>
        help "Path to logs directory. If not specified, logs are not writen on disk."

versionOption :: Parser (a -> a)
versionOption = infoOption ("disciplina-" <> (showVersion version)) $
    long "version" <>
    help "Show version."

baseKeyParamsParser :: Text -> Parser BaseKeyParams
baseKeyParamsParser who = do
    -- [Note default-cli-params]
    bkpPath <- Just <$> kpKeyPathParser
    bkpGenNew <- kpGenKeyParser
    bkpPassphrase <- kpPassphraseParser
    pure BaseKeyParams{..}
  where
    kpKeyPathParser = strOption $
         long [qc|{who}-keyfile|] <>
         metavar "FILEPATH" <>
         help [qc|Path to the secret key of the {who}. If not specified,
                 <homeDir>/{who}.key is used.|]
    kpGenKeyParser = switch $
         long [qc|{who}-gen-key|] <>
         help [qc|Generate the key and write it to '{who}-keyfile-path' path.
                 If file already exists at given path, secret in it used.|]
    kpPassphraseParser = optional . option passphraseReadM $
         long [qc|{who}-keyfile-pass|] <>
         metavar "PASSWORD" <>
         help "Password of secret key."

appDirParamParser :: Parser AppDirParam
appDirParamParser =
    AppDirectorySpecific <$> specificP <|>
    AppDirectoryOS <$ osP
  where
    specificP = strOption $
      long "appdir" <>
      metavar "FILEPATH" <>
      help "Path to application folder. To use OS-specific default folder \
           \(e. g. '%APPDIR%/Disciplina'), provide `--os-appdir` flag instead."
    osP = flag' () $
      long "os-appdir" <>
      help "Use OS-specific default application folder. To use custom application \
           \folder, provide `--appdir` option instead."

-- | Parses time with specified unit of measurement, e.g. @10s@.
timeReadM :: KnownRatName unit => ReadM (Time unit)
timeReadM = maybeReader unitsP

-- | Parses plain number to coin.
coinReadM :: ReadM Coin
coinReadM = leftToFail . coinFromInteger =<< auto @Integer

-- | Parses address.
addressReadM :: ReadM Address
addressReadM = leftToPanic . addrFromText <$> str

-- | Parses passphrase.
passphraseReadM :: ReadM PassPhrase
passphraseReadM = leftToFail . first pretty . mkPassPhrase =<< str

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

networkAddressParser :: String -> String -> Parser NetworkAddress
networkAddressParser pName helpTxt =
    option (eitherReader parseNetAddr) $
    long pName <>
    metavar "HOST:PORT" <>
    help helpTxt

clientAddressParser :: String -> String -> Parser BaseUrl
clientAddressParser pName helpTxt =
    option (eitherReader $ first displayException . parseBaseUrl) $
    long pName <>
    metavar "URL" <>
    help helpTxt

serverParamsParser :: String -> Parser ServerParams
serverParamsParser desc = do
    spAddr <- networkAddressParser (map toLower desc <> "-listen")
        ("Host/port for serving " <> desc <> " API. If executable supports \
         \multiple APIs, they are allowed to have the same port.")
    return ServerParams{..}
