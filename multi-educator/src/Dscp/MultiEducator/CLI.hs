{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE OverloadedLabels #-}

-- | CLI for educator.

module Dscp.MultiEducator.CLI
    ( multiEducatorKeyParamsParser
    , multiEducatorConfigParser
    ) where

import Data.Aeson (eitherDecodeStrict')
import Loot.Config (OptModParser, uplift, (.::), (.:<), (<*<))
import Options.Applicative (Parser, help, long, metavar, strOption, option, eitherReader)
import Servant.Client.Core (BaseUrl, parseBaseUrl)

import Dscp.Educator.CLI
import Dscp.MultiEducator.Config (MultiEducatorConfig)
import Dscp.MultiEducator.Launcher.Params (MultiEducatorKeyParams (..), MultiEducatorAAAConfig)
import Dscp.MultiEducator.Web.Educator.Auth (MultiEducatorPublicKey)
import Dscp.Witness.CLI (witnessConfigParser)

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

multiEducatorConfigParser :: OptModParser MultiEducatorConfig
multiEducatorConfigParser =
    uplift witnessConfigParser <*<
    #educator .:<
        (#db .:< postgresParamsParser <*<
         #keys .:: multiEducatorKeyParamsParser <*<
         #aaa .:< multiEducatorAAAConfigParser <*<
         #api .:< educatorWebConfigParser <*<
         #publishing .:<
             (#period .:: publishingPeriodParser
             ) <*<
         #certificates .:<
            (#resources .:: pdfResourcesPathParser)
        )
