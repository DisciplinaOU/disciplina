{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes      #-}

-- | CLI parameters of witness.

module Dscp.Witness.CLI
    ( rocksParamsParser
    , netCliParamsParser
    , netServParamsParser
    , committeeKeyParamsParser
    , witnessConfigParser
    ) where

import Loot.Config (OptModParser, (.::), (.:<), (.:-), (.:+), (<*<), (?~))
import qualified Loot.Config as L (option)
import Loot.Network.ZMQ.Common (ZTNodeId (..), parseZTNodeId)
import Options.Applicative (Parser, ReadM, auto, eitherReader, flag', help, long,
                            maybeReader, metavar, option, strOption)

import Dscp.Config (selectBranchParser)
import Dscp.CommonCLI
import Dscp.Core.Governance
import Dscp.DB.Rocks.Real.Types
import Dscp.Resource.Keys
import Dscp.Resource.Network (NetCliParams (..), NetServParams, NetServParamsRecP)
import Dscp.Util (eitherToMaybe)
import Dscp.Web.Metrics (MetricsEndpoint (..), addrToEndpoint)
import Dscp.Witness.Config
import Dscp.Witness.Keys

----------------------------------------------------------------------------
-- DB
----------------------------------------------------------------------------

rocksParamsParser :: OptModParser RocksDBParams
rocksParamsParser =
    #path  .:: pathParser <*<
    #clean .:: cleanParser
  where
    pathParser =  strOption $
        long "db-path" <>
        metavar "FILEPATH" <>
        help "Path to database directory for witness node."
    cleanParser = flag' True $
        long "db-clean" <>
        help "Clean db on every app start"

----------------------------------------------------------------------------
-- ZMQ TCP
----------------------------------------------------------------------------

-- | Parse peers to connect to.
peersParser :: Parser [ZTNodeId]
peersParser = fmap ordNub $ many $
    option (eitherReader parseZTNodeId)
    (long "peer" <>
     metavar "HOST:PORT1:PORT2" <>
     help "Peer(s) to connect to.")

-- | Parser for ZTNodeId we will bind on.
ourZTNodeIdParser :: Parser ZTNodeId
ourZTNodeIdParser = option (eitherReader parseZTNodeId)
    (long "bind" <>
     metavar "HOST:PORT1:PORT2" <>
     help "Host/ports to bind on. \
          \Two ports are needed for hosting pub/sub ZMQ sockets accordingly.")

netCliParamsParser :: Parser NetCliParams
netCliParamsParser = NetCliParams <$> peersParser

netServParamsParser :: OptModParser NetServParams
netServParamsParser = combine <$> peersParser <*> ourZTNodeIdParser <|> pure id
  where
    combine :: [ZTNodeId] -> ZTNodeId -> NetServParamsRecP -> NetServParamsRecP
    combine peers ourAddr params = params
        & L.option #peers      ?~ peers
        & L.option #ourAddress ?~ ourAddr

----------------------------------------------------------------------------
-- Metrics server
----------------------------------------------------------------------------

metricsServerParser :: Parser MetricsEndpoint
metricsServerParser =
    -- Removed 'optional'
    -- See [Note default-cli-params] in 'Dscp.CommonCLI'
    MetricsEndpoint . Just . addrToEndpoint <$>
        networkAddressParser "metrics-server" "Server to report the metrics to."

---------------------------------------------------------------------------
-- Utils
---------------------------------------------------------------------------

committeeKeyParamsParser :: OptModParser CommitteeParams
committeeKeyParamsParser = #params .:+
    (#participantN .:: nParser <*<
     selectBranchParser #paramsType openP "closed"
        (#closed .:- (#secret .:: commSecretParser))
    )
  where
    openP = flag' "open" $
      long "comm-open" <>
      help "Use open committee and become participant n/N. To use closed \
           \committee instead, provide a value for `--comm-sec` option."
    nParser = option auto
        (long "comm-n" <>
         metavar "INTEGER" <>
         help "Committee participant index. In event of secret key file \
              \generation, will be used to derive the secret.")

    commSecretParser = option committeeSecretReadM
        (long "comm-sec" <>
         metavar "BYTESTRING" <>
         help "Committee secret key. Common key for the core nodes which serves\
              \ as root key in generation of participants' secrets. To use an \
              \open committee, provide the `--comm-open` flag instead.")

-- | CLI parser for witness key params.
witnessKeysParamsParser :: OptModParser WitnessKeyParams
witnessKeysParamsParser = #params .:+
    (#paramsType .:: (basicP <|> committeeP) <*<
     #basic      .:- baseKeyParamsParser "witness" <*<
     #committee  .:- committeeKeyParamsParser
    )
  where
    basicP = flag' "basic" $
      long "witness-keys-basic" <>
      help "Use basic secret key manipulation. To use committee governance \
           \instead, provide `--witness-keys-committee` flag."
    committeeP = flag' "committee" $
      long "witness-keys-comm" <>
      help "Use committee governance to generate keys. To use basic secret keys\
           \ manipulation instead, provide `--witness-keys-basic` flag."

---------------------------------------------------------------------------
-- Readers
---------------------------------------------------------------------------

committeeSecretReadM :: ReadM CommitteeSecret
committeeSecretReadM =
    maybeReader $ eitherToMaybe . mkCommitteeSecret . fromString

---------------------------------------------------------------------------
-- Partial CLI parser for config
---------------------------------------------------------------------------

witnessConfigParser :: OptModParser WitnessConfig
witnessConfigParser = #witness .:<
    (#db .:< rocksParamsParser <*<
     #network .:< netServParamsParser <*<
     #keys .:< witnessKeysParamsParser <*<
     #api .:<
        (#maybe .:+ selectBranchParser #maybeType nothingP "just"
            (#just .:- serverParamsParser "Witness")
        ) <*<
     #appDir .:< appDirParamParser <*<
     #metricsEndpoint .:: metricsServerParser)
  where
    nothingP = flag' "nothing" $
      long "witness-no-server" <>
      help "Avoids to start a (servant) server. To start one instead, provide a\
           \ value for the `--witness-listen` option."
