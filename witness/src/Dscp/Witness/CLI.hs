{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes      #-}

-- | CLI parameters of witness.

module Dscp.Witness.CLI
    ( rocksParamsParser
    , netCliParamsParser
    , netServParamsParser
    , committeeParamsParser
    , witnessConfigParser
    ) where

import qualified Data.Set as Set
import Loot.Config (OptParser, (.::), (.:<), (.<>))
import Loot.Network.ZMQ.Common (PreZTNodeId (..), parsePreZTNodeId)
import Options.Applicative (Parser, ReadM, auto, eitherReader, help, long, maybeReader, metavar,
                            option, strOption)

import Dscp.CommonCLI (appDirParamParser, baseKeyParamsParser, logParamsParser,
                       networkAddressParser, serverParamsParser)
import Dscp.Core.Governance
import Dscp.DB.Rocks.Real.Types (RocksDBParams (..))
import Dscp.Resource.Keys
import Dscp.Resource.Network (NetCliParams (..), NetServParams (..))
import Dscp.Util (eitherToMaybe)
import Dscp.Web.Metrics (MetricsEndpoint (..), addrToEndpoint)
import Dscp.Witness.Config
import Dscp.Witness.Keys

----------------------------------------------------------------------------
-- DB
----------------------------------------------------------------------------

rocksParamsParser :: Parser RocksDBParams
rocksParamsParser = fmap RocksDBParams $ strOption $
    long "db-path" <>
    metavar "FILEPATH" <>
    help "Path to database directory for witness node. If not specified, \
         \'witness-db' directory is used."
    -- Removed default 'witness-db' value
    -- See [Note default-cli-params] in 'Dscp.CommonCLI'

----------------------------------------------------------------------------
-- ZMQ TCP
----------------------------------------------------------------------------

-- | Parse peers to connect to.
peersParser :: Parser (Set PreZTNodeId)
peersParser = fmap Set.fromList . many $
    option (eitherReader parsePreZTNodeId)
    (long "peer" <>
     metavar "HOST:PORT1:PORT2" <>
     help "Peer(s) we should connect to.")

-- | Parser for ZTNodeId we will bind on.
ourZTNodeIdParser :: Parser PreZTNodeId
ourZTNodeIdParser = option (eitherReader parsePreZTNodeId)
    (long "bind" <>
     metavar "HOST:PORT1:PORT2" <>
     help "Host/ports to bind on, also the public address we share with other nodes. \
          \Two ports are needed for hosting pub/sub ZMQ sockets accordingly.")

-- | Parser for ZTNodeId we will bind on.
internalZTNodeIdParser :: Parser PreZTNodeId
internalZTNodeIdParser = option (eitherReader parsePreZTNodeId)
    (long "bind-internal" <>
     metavar "HOST:PORT1:PORT2" <>
     help "Overrides address to bind on, still the --bind value must be addressable \
          \and designates publically visible address. Use this option \
          \when the node is behind NAT.")

netCliParamsParser :: Parser NetCliParams
netCliParamsParser = NetCliParams <$> peersParser

netServParamsParser :: Parser NetServParams
netServParamsParser =
    NetServParams <$> peersParser <*> ourZTNodeIdParser <*> optional internalZTNodeIdParser

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

committeeParamsParser :: Parser CommitteeParams
committeeParamsParser =
    combine <$> nParser <*> optional commSecretParser
  where
    combine cpParticipantN Nothing         = CommitteeParamsOpen {..}
    combine cpParticipantN (Just cpSecret) = CommitteeParamsClosed {..}

    nParser = option auto
        (long "comm-n" <>
         metavar "INTEGER" <>
         help "Committee participant index. In event of secret key file \
              \generation, will be used to derive the secret.")

    commSecretParser = option committeeSecretReadM
        (long "comm-sec" <>
         metavar "BYTESTRING" <>
         help "Committee secret key. Common key for the core nodes \
              \which serves as root key in generation of participants' secrets.")

witnessKeyParamsParser :: Parser WitnessKeyParams
witnessKeyParamsParser =
    Committee <$> committeeParamsParser <|>
    Basic <$> baseKeyParamsParser "witness"

---------------------------------------------------------------------------
-- Readers
---------------------------------------------------------------------------

committeeSecretReadM :: ReadM CommitteeSecret
committeeSecretReadM =
    maybeReader $ eitherToMaybe . mkCommitteeSecret . fromString

---------------------------------------------------------------------------
-- Partial CLI parser for config
---------------------------------------------------------------------------

witnessConfigParser :: OptParser WitnessConfig
witnessConfigParser = #witness .:<
    (#logging .:: logParamsParser "witness" .<>
     #db .:: rocksParamsParser .<>
     #network .:: netServParamsParser .<>
     #keys .:: witnessKeyParamsParser .<>
     #api .:: (Just <$> serverParamsParser "Witness") .<>
     #appDir .:: appDirParamParser .<>
     #metricsEndpoint .:: metricsServerParser)
