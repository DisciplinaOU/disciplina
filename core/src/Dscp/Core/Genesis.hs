-- | Genesis info and related.

module Dscp.Core.Genesis
    ( GenesisInfo (..)
    , GenesisDistributionElem (..)
    , GenesisDistribution (..)
    , GenesisConfig (..)
    , GenAddressMap (..)
    , distrToMap
    , formGenesisInfo
    ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Map.Strict as Map

import Dscp.Core.Foundation
import Dscp.Core.Governance
import Dscp.Crypto (keyGen, sign, unsafeHash, withIntSeed)


-- | Wrapper over address mapping.
newtype GenAddressMap = GenAddressMap
    { unGenAddressMap :: Map Address Coin
    } deriving (Eq, Ord, Show, Generic)

instance Semigroup GenAddressMap where
    GenAddressMap a <> GenAddressMap b =
        GenAddressMap (M.unionWith sumCoins a b)

instance Monoid GenAddressMap where
    mempty = GenAddressMap mempty
    mappend = (<>)

-- | Runtime representation of the genesis info. It is built from
-- other config parameters and genesis config in particular. It is
-- then put back into the config.
data GenesisInfo = GenesisInfo
    { giAddressMap   :: GenAddressMap
    , giGenesisBlock :: Block
    } deriving (Eq, Show, Generic)

-- | Coin distribution.
data GenesisDistributionElem
    = GDEqual Coin             -- ^ Equally distributed c coins among n persons.
    | GDSpecific GenAddressMap -- ^ Specific address map.
    deriving (Eq, Show, Generic)

newtype GenesisDistribution = GenesisDistribution
    { genesisDistributionElems :: NonEmpty GenesisDistributionElem
    } deriving (Eq, Show, Generic)

-- | Genesis configuration.
data GenesisConfig = GenesisConfig
    { gcGenesisSeed  :: Text
      -- ^ Seed that will be used in the creation of genesis block.
    , gcGovernance   :: Governance
      -- ^ Type of governance used.
    , gcDistribution :: GenesisDistribution
      -- ^ Initial coins distribution.
    } deriving (Eq, Show, Generic)

distrElemToMap :: Maybe (NonEmpty Address) -> GenesisDistributionElem -> GenAddressMap
distrElemToMap _ (GDSpecific addrMap) = addrMap
distrElemToMap (Just addrs) (GDEqual cTotal) =
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
distrElemToMap _ _ = error "distrToMap: param combination is invalid"

distrToMap :: Maybe (NonEmpty Address) -> GenesisDistribution -> GenAddressMap
distrToMap maddrs (GenesisDistribution distrs) =
    foldl1 mappend $ fmap (distrElemToMap maddrs) distrs

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
