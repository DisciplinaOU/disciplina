{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes   #-}

-- | Common CLI params.

module Dscp.CommonCLI
       ( logParamsParser
       , versionOption
       , baseKeyParamsParser
       , timeReadM
       , coinReadM
       , networkAddressParser
       , clientAddressParser
       , serverParamsParser
       , appDirParamParser
       ) where

import Data.Char (toLower)
import Data.Version (showVersion)
import qualified Loot.Log as Log
import Options.Applicative (Parser, ReadM, auto, eitherReader, help, infoOption, long, metavar,
                            option, str, strOption, switch)
import Servant.Client (BaseUrl (..), parseBaseUrl)
import Text.InterpolatedString.Perl6 (qc)
import Text.Parsec (eof, many1, parse, sepBy)
import Text.Parsec.Char (char, digit)
import qualified Text.Parsec.String as Parsec
import Time.Rational (KnownRat)
import Time.Units (Microsecond, Millisecond, Minute, Second, Time, toUnit)

import Dscp.Core.Foundation.Witness
import Dscp.Crypto (mkPassPhrase)
import Dscp.Resource.AppDir
import Dscp.Resource.Keys (BaseKeyParams (..))
import Dscp.Resource.Logging (LoggingParams (..))
import Dscp.Util (leftToFail)
import Dscp.Web (NetworkAddress (..), ServerParams (..))
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
    bkpPath <- kpKeyPathParser
    bkpGenNew <- kpGenKeyParser
    bkpPassphrase <- kpPassphraseParser
    pure BaseKeyParams{..}
  where
    kpKeyPathParser = optional . strOption $
         long [qc|{who}-keyfile|] <>
         metavar "FILEPATH" <>
         help [qc|Path to the secret key of the {who}. If not specified, \
                 \<homeDir>/{who}.key is used.|]
    kpGenKeyParser = switch $
         long [qc|{who}-gen-key|] <>
         help [qc|Generate the key and write it to '{who}-keyfile-path' path.
                 If file already exists at given path, secret in it used.|]
    kpPassphraseParser = optional . option passphraseReadM $
         long [qc|{who}-keyfile-pass|] <>
         metavar "PASSWORD" <>
         help "Password of secret key."
    passphraseReadM = leftToFail . first pretty . mkPassPhrase =<< str

appDirParamParser :: Parser AppDirParam
appDirParamParser = AppDirectorySpecific <$>
                        (strOption $
                        long "appdir" <>
                        metavar "FILEPATH" <>
                        help "Path to application folder. If not specified, \
                             \OS-dependent folder for applications will be \
                             \used, for instance '%APPDIR%/Disciplina'.") <|>
                    pure AppDirectoryOS

-- | Parses time with specified unit of measurement, e.g. @10s@.
timeReadM :: KnownRat unit => ReadM (Time unit)
timeReadM = asum
    [ toUnit @_ @Second <$> auto
    , toUnit @_ @Millisecond <$> auto
    , toUnit @_ @Microsecond <$> auto
    , toUnit @_ @Minute <$> auto
    ]

-- | Parses plain number to coin.
coinReadM :: ReadM Coin
coinReadM = leftToFail . coinFromInteger =<< auto @Integer

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

parseNetAddr :: String -> Either String NetworkAddress
parseNetAddr st =
    first niceError $ parse parseNA "" st
  where
    niceError = const "Invalid Network Address"
    parseNA :: Parsec.Parser NetworkAddress
    parseNA = NetworkAddress <$> parseHost <* char ':'
                             <*> parsePort <* eof
    parseHost = do host <- parseByte `sepBy` (char '.')
                   unless (length host == 4) $ fail "invalid"
                   return $ toText $ intercalate "." $ (map show host)
    parsePort = parseWord 16
    parseByte = parseWord 8 :: Parsec.Parser Integer
    parseWord n = do x <- fromMaybe (error "unexpected") . readMaybe <$> many1 digit
                     when ((x :: Integer) > 2 ^ (n :: Integer) - 1) $ fail "invalid"
                     return $ fromIntegral x

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
        ("Host/port for serving " <> desc <> " API.")
    return ServerParams{..}
