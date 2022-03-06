{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE OverloadedLabels #-}

-- | CLI for educator.

module Dscp.MultiEducator.CLI
    ( multiEducatorKeyParamsParser
    , multiEducatorConfigParser
    ) where

import Universum

import Data.Aeson (eitherDecodeStrict')
import Loot.Config (OptModParser, (.::), (.:<), (<*<))
import Options.Applicative (Parser, eitherReader, help, long, metavar, option, strOption)
import Servant.Client.Core (BaseUrl, parseBaseUrl)

import Dscp.CommonCLI
import Dscp.Educator.CLI
import Dscp.Educator.Web.Auth
import Dscp.MultiEducator.Config
import Dscp.MultiEducator.Launcher.Params (MultiEducatorAAAConfig, MultiEducatorKeyParams (..))
import Dscp.MultiEducator.Web.Educator.Auth (MultiEducatorPublicKey, educatorAuthLoginSimple)

import qualified Data.ByteString as BS

multiEducatorKeyParamsParser :: Parser MultiEducatorKeyParams
multiEducatorKeyParamsParser = MultiEducatorKeyParams <$>
    strOption (long "educator-key-dir" <>
               metavar "PATH" <>
               help "Path to the directory with educator keys")

multiEducatorAAAConfigParser :: OptModParser MultiEducatorAAAConfig
multiEducatorAAAConfigParser =
    #serviceUrl .:: serviceUrlParser <*<
    #publicKey  .:: publicKeyParser
  where
    serviceUrlParser :: Parser BaseUrl
    serviceUrlParser =
        option (eitherReader $ first displayException . parseBaseUrl) $
        long "aaa-service-url" <>
        metavar "URL" <>
        help "Url of the AAA microservice"
    publicKeyParser :: Parser MultiEducatorPublicKey
    publicKeyParser =
        option (eitherReader $ eitherDecodeStrict' . addQuotationMarks . fromString) $
        long "aaa-public-key" <>
        help "Public key of the AAA microservice"
      where
        addQuotationMarks bs = BS.intercalate bs ["\"", "\""]

multiEducatorApiNoAuthParser :: Parser (NoAuthContext "multi-educator")
multiEducatorApiNoAuthParser = noAuthContextParser . fmap educatorAuthLoginSimple . strOption $
    long "educator-api-no-auth" <>
    help "Make authentication into Educator API of multi-educator optional. \
         \Accepts educator id used when request is unauthenticated."

multiEducatorWebConfigParser :: OptModParser MultiEducatorWebConfig
multiEducatorWebConfigParser =
    #serverParams .:< serverParamsParser "Educator" <*<
    #multiEducatorAPINoAuth .:: multiEducatorApiNoAuthParser <*<
    #studentAPINoAuth .:: studentApiNoAuthParser

multiEducatorConfigParser :: OptModParser MultiEducatorConfig
multiEducatorConfigParser =
    #educator .:<
        (#db .:< postgresParamsParser <*<
         #appDir .:< appDirParamParser <*<
         #keys .:: multiEducatorKeyParamsParser <*<
         #aaa .:< multiEducatorAAAConfigParser <*<
         #api .:< multiEducatorWebConfigParser <*<
         #publishing .:<
             (#period .:: publishingPeriodParser
             ) <*<
         #certificates .:<
            (#resources .:: pdfResourcesPathParser)
        )
