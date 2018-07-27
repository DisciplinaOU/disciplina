{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes   #-}

-- | CLI parameters of witness.

module Dscp.Witness.CLI
    ( rocksParamsParser
    , netCliParamsParser
    , netServParamsParser

    , witnessParamsParser
    ) where

import qualified Data.Set as Set
import Loot.Network.ZMQ.Common (ZTNodeId (..), parseZTNodeId)
import Options.Applicative (Parser, eitherReader, help, long, metavar, option, strOption, value)

import Dscp.CommonCLI (keyParamsParser, logParamsParser, serverParamsParser)
import Dscp.DB.Rocks.Real.Types (RocksDBParams (..))
import Dscp.Resource.Network (NetCliParams (..), NetServParams (..))
import Dscp.Witness.Launcher.Params (WitnessParams (..))

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
peersParser :: Parser (Set ZTNodeId)
peersParser =
    fmap Set.fromList $
    many $
    option (eitherReader parseZTNodeId)
           (long "peer" <> metavar "HOST:PORT1:PORT2" <> help "Peer(s) we should connect to")

-- | Parser for ZTNodeId we will bind on.
ourZTNodeIdParser :: Parser ZTNodeId
ourZTNodeIdParser = do
    option (eitherReader parseZTNodeId)
           (long "bind" <> metavar "HOST:PORT1:PORT2" <> help "Host/ports to bind on")

netCliParamsParser :: Parser NetCliParams
netCliParamsParser = NetCliParams <$> peersParser

netServParamsParser :: Parser NetServParams
netServParamsParser = NetServParams <$> peersParser <*> ourZTNodeIdParser

----------------------------------------------------------------------------
-- Witness params parser
----------------------------------------------------------------------------

witnessParamsParser :: Parser WitnessParams
witnessParamsParser = do
    wpLoggingParams <- logParamsParser "witness"
    wpDBParams <- rocksParamsParser
    wpNetworkParams <- netServParamsParser
    wpKeyParams <- keyParamsParser "witness"
    wpWalletServerParams <- serverParamsParser "Witness"
    pure $ WitnessParams {..}
