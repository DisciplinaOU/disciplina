-- | Genesis info and related.

module Dscp.Core.Genesis
    ( GenesisInfo (..)
    , GenesisDistribution (..)
    , GenesisConfig (..)
    , GenAddressMap (..)
    , distrToMap
    , formGenesisInfo
    ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

import Dscp.Core.Foundation
import Dscp.Core.Governance
import Dscp.Crypto (keyGen, sign, unsafeHash, withIntSeed)


-- | Wrapper over address mapping.
newtype GenAddressMap = GenAddressMap
    { unGenAddressMap :: Map Address Coin
    } deriving (Eq, Ord, Monoid, Show, Generic)

-- | Runtime representation of the genesis info. It is built from
-- other config parameters and genesis config in particular. It is
-- then put back into the config.
data GenesisInfo = GenesisInfo
    { giAddressMap   :: GenAddressMap
    , giGenesisBlock :: Block
    } deriving (Eq, Show, Generic)

-- | Coin distribution. TODO come up with a way to combine these.
data GenesisDistribution
    = GDEqual Coin             -- ^ Equally distributed c coins among n persons.
    | GDSpecific GenAddressMap -- ^ Specific address map.
    deriving (Eq, Show, Generic)

-- | Genesis configuration.
data GenesisConfig = GenesisConfig
    { gcGenesisSeed  :: Text
      -- ^ Seed that will be used in the creation of genesis block.
    , gcGovernance   :: Governance
      -- ^ Type of governance used.
    , gcDistribution :: GenesisDistribution
      -- ^ Initial coins distribution.
    } deriving (Eq, Show, Generic)

distrToMap :: Maybe (NonEmpty Address) -> GenesisDistribution -> GenAddressMap
distrToMap Nothing (GDSpecific addrMap) = addrMap
distrToMap (Just addrs) (GDEqual cTotal) =
    let mapping = case addrs of
            a :| [] -> [(a, cTotal)]
            a :| as -> let n = length addrs
                           c = fromIntegral $ unCoin cTotal
                           each' = c `div` n
                       in (a, unsafeMkCoin $ c - (n-1) * each') :
                          map (, unsafeMkCoin each') as
    in if sum (map (unCoin . snd) mapping) /= unCoin cTotal
       then error "distrToMap: equal summing failed"
       else GenAddressMap $ Map.fromList mapping
distrToMap _ _ = error "distrToMap: param combination is invalid"

formGenesisInfo :: GenesisConfig -> GenesisInfo
formGenesisInfo GenesisConfig{..} =
    GenesisInfo { giAddressMap = genesisAddrMap
                , giGenesisBlock = genesisBlock
                }
  where
    govAddresses = case gcGovernance of
        GovCommittee com -> committeeAddrs com
        GovOpen          -> error "formGenesisInfo: open governance is not implemented"

    genesisAddrMap = distrToMap (Just $ NE.fromList govAddresses) gcDistribution

    genesisBlock :: Block
    genesisBlock = Block header payload
      where
        (sk,pk) = withIntSeed 12345 keyGen

        createTx (a, c) i =
            let fromAddr = mkAddr pk
                twTx = Tx { txInAcc = TxInAcc fromAddr i
                          , txInValue = c
                          , txOuts = [TxOut a c] }
                twWitness = TxWitness { txwSig = sign sk (toTxId twTx, pk, ())
                                      , txwPk = pk }
            in GMoneyTxWitnessed $ TxWitnessed {..}
        txs = map (uncurry createTx) (Map.toList (unGenAddressMap genesisAddrMap) `zip` [0..])
        payload = BlockBody txs


        prevHash = unsafeHash (gcGenesisSeed :: Text)
        toSign = BlockToSign 0 0 prevHash payload
        header = Header { hSignature = sign sk toSign
                        , hIssuer = pk
                        , hSlotId = 0
                        , hDifficulty = 0
                        , hPrevHash = prevHash
                        }
