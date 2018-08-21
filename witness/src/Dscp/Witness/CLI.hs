{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes   #-}

-- | CLI parameters of witness.

module Dscp.Witness.CLI
    ( rocksParamsParser
    , netCliParamsParser
    , netServParamsParser
    , committeeParamsParser

    , witnessParamsParser
    ) where

import qualified Data.Set as Set
import Loot.Network.ZMQ.Common (PreZTNodeId (..), parsePreZTNodeId)
import Options.Applicative (Parser, auto, eitherReader, help, long, metavar, option, strOption,
                            value)

import Dscp.CommonCLI (baseKeyParamsParser, logParamsParser, serverParamsParser, appDirParamParser)
import Dscp.Core.Governance (CommitteeSecret (..))
import Dscp.DB.Rocks.Real.Types (RocksDBParams (..))
import Dscp.Resource.Keys
import Dscp.Resource.Network (NetCliParams (..), NetServParams (..))
import Dscp.Witness.Launcher.Params

----------------------------------------------------------------------------
-- DB
----------------------------------------------------------------------------

rocksParamsParser :: Parser RocksDBParams
rocksParamsParser = fmap RocksDBParams $ strOption $
    long "db-path" <>
    metavar "FILEPATH" <>
    help "Path to database directory for witness node." <>
    value "witness-db"

----------------------------------------------------------------------------
-- ZMQ TCP
----------------------------------------------------------------------------

-- | Parse peers to connect to.
peersParser :: Parser (Set PreZTNodeId)
peersParser =
    fmap Set.fromList $
    many $
    option (eitherReader parsePreZTNodeId)
           (long "peer" <> metavar "HOST:PORT1:PORT2" <> help "Peer(s) we should connect to.")

-- | Parser for ZTNodeId we will bind on.
ourZTNodeIdParser :: Parser PreZTNodeId
ourZTNodeIdParser = do
    option (eitherReader parsePreZTNodeId)
           (long "bind" <>
            metavar "HOST:PORT1:PORT2" <>
            help "Host/ports to bind on, also the public address we share with other nodes. \
                 \Two ports are needed for hosting pub/sub ZMQ sockets accordingly.")

-- | Parser for ZTNodeId we will bind on.
internalZTNodeIdParser :: Parser PreZTNodeId
internalZTNodeIdParser = do
    option (eitherReader parsePreZTNodeId)
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

---------------------------------------------------------------------------
-- Utils
---------------------------------------------------------------------------

committeeParamsParser :: Parser CommitteeParams
committeeParamsParser =
    combine <$> nParser <*> optional commSecretParser
  where
    combine cpParticipantN Nothing         = CommitteeParamsOpen {..}
    combine cpParticipantN (Just cpSecret) = CommitteeParamsClosed {..}

    nParser =
        option auto
            (long "comm-n" <> metavar "INTEGER" <>
             help "Committee participant index. In event of secret key file \
                  \generation, will be used to derive the secret.")

    commSecretParser =
        fmap CommitteeSecret $
        strOption
            (long "comm-sec" <> metavar "BYTESTRING" <>
             help "Committee secret key. Common key for the core nodes \
                  \which serves as root key in generation of participants' secrets.")

witnessKeyParamsParser :: Parser WitnessKeyParams
witnessKeyParamsParser = do
    wkpBase <- baseKeyParamsParser "witness"
    wkpCommittee <- optional committeeParamsParser
    pure $ WitnessKeyParams {..}

----------------------------------------------------------------------------
-- Witness params parser
----------------------------------------------------------------------------

witnessParamsParser :: Parser WitnessParams
witnessParamsParser = do
    wpLoggingParams <- logParamsParser "witness"
    wpDBParams <- rocksParamsParser
    wpNetworkParams <- netServParamsParser
    wpKeyParams <- witnessKeyParamsParser
    wpWalletServerParams <- serverParamsParser "Witness"
    wpAppDirParam <- appDirParamParser
    pure $ WitnessParams {..}
