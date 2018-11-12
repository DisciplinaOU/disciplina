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

import Loot.Config (ModParser, OptModParser, modifying, (%::), (..:), (.::), (.:<), (<*<))
import Loot.Network.ZMQ.Common (ZTNodeId (..), parseZTNodeId)
import Options.Applicative (Parser, ReadM, auto, eitherReader, flag', help, long, maybeReader,
                            metavar, option, strOption)

import Dscp.CommonCLI
import Dscp.Core.Governance
import Dscp.DB.Rocks.Real.Types
import Dscp.Resource.Keys
import Dscp.Resource.Network (NetCliParams (..), NetServParams (..))
import Dscp.Util (eitherToMaybe)
import Dscp.Web.Metrics (MetricsEndpoint (..), addrToEndpoint)
import Dscp.Witness.Config
import Dscp.Witness.Keys

----------------------------------------------------------------------------
-- DB
----------------------------------------------------------------------------

rocksParamsParser :: ModParser RocksDBParams
rocksParamsParser =
    rdpPathL  ..: pathParser <*<
    rdpCleanL ..: cleanParser
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

netServParamsParser :: Parser NetServParams
netServParamsParser = NetServParams <$> peersParser <*> ourZTNodeIdParser

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

-- | CLI parser for witness key params.
-- Note [DSCP-347]: there is a bug -- one cannot redefine committee secret keys
-- with basic key params in CLI. Such usecase is not very likely, but
-- this is still counterintuitive.
witnessKeyParamsParser :: ModParser WitnessKeyParams
witnessKeyParamsParser =
    modifying (Committee <$> committeeParamsParser) <|>
    over _Basic <$> baseKeyParamsParser "witness"

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
    (#db %:: rocksParamsParser <*<
     #network .:: netServParamsParser <*<
     #keys %:: witnessKeyParamsParser <*<
     #api .:: (Just <$> serverParamsParser "Witness") <*<
     #appDir .:: appDirParamParser <*<
     #metricsEndpoint .:: metricsServerParser)
