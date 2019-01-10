{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

-- | Genesis info and related.

module Dscp.Core.Genesis
    ( GenesisInfo (..)
    , GenesisDistributionElem (..)
    , GenesisDistribution (..)
    , GenesisConfig
    , GenesisConfigRec
    , GenesisConfigRecP
    , GenAddressMap (..)
    , totalCoinsAddrMap
    , distrToMap
    , genesisSk
    , formGenesisInfo
    ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Map.Strict as Map
import Loot.Config ((:::), PartialConfig, Config, option)

import Dscp.Core.Foundation
import Dscp.Core.Governance
import Dscp.Crypto
import Dscp.Util

-- | Wrapper over address mapping.
newtype GenAddressMap = GenAddressMap
    { unGenAddressMap :: Map Address Coin
    } deriving (Eq, Ord, Show, Generic)

instance Semigroup GenAddressMap where
    GenAddressMap a <> GenAddressMap b =
        GenAddressMap (M.unionWith unsafeAddCoin a b)

instance Monoid GenAddressMap where
    mempty = GenAddressMap mempty
    mappend = (<>)

-- | Total coins in address map.
totalCoinsAddrMap :: GenAddressMap -> Coin
totalCoinsAddrMap (GenAddressMap m) = leftToPanic $ sumCoins (Map.elems m)

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
type GenesisConfig =
    '[ "genesisSeed"  ::: Text
       -- Seed that will be used in the creation of genesis block.
     , "governance"   ::: Governance
       -- Type of governance used.
     , "distribution" ::: GenesisDistribution
       -- Initial coins distribution.
     ]

type GenesisConfigRec = Config GenesisConfig
type GenesisConfigRecP = PartialConfig GenesisConfig

distrElemToMap :: Maybe (NonEmpty Address) -> GenesisDistributionElem -> GenAddressMap
distrElemToMap _ (GDSpecific addrMap) = addrMap
distrElemToMap (Just addrs) (GDEqual cTotal) =
    let mapping = case addrs of
            a :| [] -> [(a, cTotal)]
            a :| as -> let n = length addrs
                           c = fromIntegral $ unCoin cTotal
                           each' = c `div` n
                       in (a, unsafeMkCoin $ c - (n - 1) * each') :
                          map (, unsafeMkCoin each') as
    in if sum (map (unCoin . snd) mapping) /= unCoin cTotal
       then error "distrToMap: equal summing failed"
       else GenAddressMap $ Map.fromList mapping
distrElemToMap _ _ = error "distrToMap: param combination is invalid"

distrToMap :: Maybe (NonEmpty Address) -> GenesisDistribution -> GenAddressMap
distrToMap maddrs (GenesisDistribution distrs) =
    foldl1 mappend $ fmap (distrElemToMap maddrs) distrs

-- | Genesis secret key.
genesisSk :: SecretKey
genesisSk = withIntSeed 12345 genSecretKey

-- | Generate genesis info (addr map, block).
formGenesisInfo :: GenesisConfigRec -> GenesisInfo
formGenesisInfo genesisConfig =
    GenesisInfo { giAddressMap = genesisAddrMap
                , giGenesisBlock = genesisBlock
                }
  where
    govAddresses = case genesisConfig ^. option #governance of
        GovCommittee com -> committeeAddrs com

    genesisAddrMap = distrToMap (Just $ NE.fromList govAddresses) $
        genesisConfig ^. option #distribution

    genesisBlock :: Block
    genesisBlock = Block header payload
      where
        sk = genesisSk
        pk = toPublic sk

        initTx =
            let createTxOut (a, c) = TxOut a c
                fromAddr = mkAddr pk
                txOutputs = map createTxOut (Map.toList $ unGenAddressMap genesisAddrMap)
                twTx = Tx { txInAcc = TxInAcc fromAddr 0
                          , txInValue = totalCoinsAddrMap genesisAddrMap
                          , txOuts = txOutputs }
                twWitness = TxWitness { txwSig = sign sk (toTxId twTx, pk, ())
                                      , txwPk = pk }
            in GMoneyTxWitnessed $ TxWitnessed {..}

        payload = BlockBody [initTx]

        prevHash = unsafeHash $ genesisConfig ^. option #genesisSeed
        toSign = BlockToSign 0 0 prevHash (hash payload)
        header = Header { hSignature = sign sk toSign
                        , hIssuer = pk
                        , hSlotId = 0
                        , hDifficulty = 0
                        , hPrevHash = prevHash
                        }
