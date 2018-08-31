-- | Genesis info and related.

module Dscp.Core.Genesis
    ( GenesisInfo (..)
    , GenesisDistributionElem (..)
    , GenesisDistribution (..)
    , GenesisConfig (..)
    , GenAddressMap (..)
    , totalCoinsAddrMap
    , distrToMap
    , genesisSk
    , formGenesisInfo
    ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Map.Strict as Map

import Dscp.Core.Foundation
import Dscp.Core.Governance
import Dscp.Crypto


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

-- | Total coins in address map.
totalCoinsAddrMap :: GenAddressMap -> Coin
totalCoinsAddrMap (GenAddressMap m) = foldr unsafeAddCoin (Coin 0) (Map.elems m)

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

genesisSk :: SecretKey
genesisSk = withIntSeed 12345 genSecretKey

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
        sk = genesisSk
        pk = toPublic sk

        createTxOut (a, c) = TxOut a c

        initTx =
            let fromAddr = mkAddr pk
                txOutputs = map createTxOut (Map.toList $ unGenAddressMap genesisAddrMap)
                twTx = Tx { txInAcc = TxInAcc fromAddr 0
                          , txInValue = totalCoinsAddrMap genesisAddrMap
                          , txOuts = txOutputs }
                twWitness = TxWitness { txwSig = sign sk (toTxId twTx, pk, ())
                                      , txwPk = pk }
            in GMoneyTxWitnessed $ TxWitnessed {..}

        payload = BlockBody [initTx]

        prevHash = unsafeHash (gcGenesisSeed :: Text)
        toSign = BlockToSign 0 0 prevHash (hash payload)
        header = Header { hSignature = sign sk toSign
                        , hIssuer = pk
                        , hSlotId = 0
                        , hDifficulty = 0
                        , hPrevHash = prevHash
                        }
