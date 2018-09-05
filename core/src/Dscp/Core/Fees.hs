{-# LANGUAGE StrictData #-}

-- | Trnsaction fees processing and calculation.
module Dscp.Core.Fees
       ( Fees (..)
       , FeeCoefficients (..)
       , calculateFee
       , noFees
       , fixFees
       , precomputePublicationSize
       ) where

import Codec.Serialise (Serialise (..))

import Dscp.Core.Foundation
import Dscp.Crypto
import Dscp.Util

-- | Amount of transaction fees.
newtype Fees = Fees { unFees :: Coin }
    deriving (Eq, Ord, Show, Generic, Hashable, Bounded)

instance Serialise Fees

-- | Setup for fee calculation.
data FeeCoefficients = FeeCoefficients
    { fcMultiplier :: Float   -- ^ How much tokens taken per byte of tx?
    , fcMinimal    :: Coin    -- ^ How much is the base fee?
    } deriving (Eq, Show)

-- | Calculate fees from tx size.
calculateFee :: FeeCoefficients -> Size Serialise GTxWitnessed -> Fees
calculateFee FeeCoefficients { fcMultiplier, fcMinimal } (Size size) =
    Fees $ Coin $ unCoin fcMinimal + round (fcMultiplier * fromIntegral size)

-- | "No fee" setup.
noFees :: FeeCoefficients
noFees = FeeCoefficients
    { fcMultiplier = 0
    , fcMinimal    = Coin 0
    }

-- | Pick fees which would fit for given transaction.
-- Fits only the case when transaction size grows monotonically with fees.
--
-- [DSCP-252]: this function would work only for linear fees policy,
-- for other policies another algorithm would be required.
fixFees :: FeeCoefficients -> (res -> GTxWitnessed) -> (Fees -> res) -> res
fixFees coefs toFullTx toResTx = go (Fees $ fcMinimal coefs)
  where
    go fees =
        let tx = toResTx fees
            size = sizeSerialised (toFullTx tx)
            fees' = calculateFee coefs size
        in case fees `compare` fees' of
            EQ -> tx
            LT -> go fees'
            GT -> error "fixFees: fees unexpectedly decreased"

-- | Precomputes publication tx size. For now, it only depends on
-- whether the private block is the first one.
precomputePublicationSize :: Bool -> Size Serialise GTxWitnessed
precomputePublicationSize firstBlock =
    let (sk, pk) = withIntSeed 0 keyGen
        addr = mkAddr pk
        hHash = unsafeHash ()
        prevBlock = bool Nothing (Just hHash) firstBlock
        pubTx = PublicationTx addr prevBlock hHash
        pub = Publication hHash prevBlock
        sig = sign sk (toPtxId pubTx, pk, pub)
        pubWitness = PublicationTxWitness sig pk
        gtxWitnessed = GPublicationTxWitnessed $ PublicationTxWitnessed pubTx pubWitness
    in sizeSerialised gtxWitnessed
