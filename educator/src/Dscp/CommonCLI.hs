{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes      #-}

-- | Common CLI params.

module Dscp.CommonCLI
       ( versionOption
       , passphraseReadM
       , timeReadM
       , coinReadM
       , addressReadM
       , networkAddressParser
       , clientAddressParser
       , serverParamsParser
       , appDirParamParser
       ) where

import Universum

import Fmt (pretty)
import Data.Char (toLower)
import Data.Version (showVersion)
import Loot.Config.CLI (OptModParser, (.:+), (.:-), (.::))
import Options.Applicative (Parser, ReadM, auto, eitherReader, flag', help, infoOption, long,
                            maybeReader, metavar, option, str, strOption)
import Servant.Client (BaseUrl (..), parseBaseUrl)
import Time (KnownRatName, Time, unitsP)

import Dscp.Config (selectBranchParser)
import Dscp.Core.Foundation
import Dscp.Crypto (PassPhrase, mkPassPhrase)
import Dscp.Resource.AppDir
import Dscp.Util
import Dscp.Web (NetworkAddress (..), ServerParams, parseNetAddr)
import Paths_disciplina_educator (version)

versionOption :: Parser (a -> a)
versionOption = infoOption ("disciplina-" <> (showVersion version)) $
    long "version" <>
    help "Show version."

appDirParamParser :: OptModParser AppDirParam
appDirParamParser = #param .:+
    selectBranchParser #paramType osP "specific" (#specific .:- (#path .:: specificP))
  where
    specificP = strOption $
      long "appdir" <>
      metavar "FILEPATH" <>
      help "Path to application folder. To use OS-specific default folder \
           \(e. g. '%APPDIR%/Disciplina'), provide `--os-appdir` flag instead."
    osP = flag' "os" $
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
passphraseReadM = leftToFail @Text . first pretty . mkPassPhrase =<< str

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

serverParamsParser :: String -> OptModParser ServerParams
serverParamsParser desc =
    #addr .:: networkAddressParser (map toLower desc <> "-listen")
        ("Host/port for serving " <> desc <> " API. If executable supports \
         \multiple APIs, they are allowed to have the same port.")
