{-# LANGUAGE StrictData #-}

-- | Trnsaction fees processing and calculation.
module Dscp.Core.Fees
       ( Fees (..)
       , FeeCoefficients (..)
       , calcFeeTx
       , calcFeePub
       , calcFeeG
       , noFees
       , fixFees
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
    { fcMinimal       :: Coin  -- ^ How much is the base fee?
    , fcMultiplier    :: Float -- ^ How much tokens taken per byte of tx?
    , fcMultiplierPub :: Float -- ^ The multiplier for publications,
                               -- gets multipled by the number of txs
                               -- in the private block.
    } deriving (Eq, Show)

-- | Calculate fees from tx size.
calcFeeTx :: FeeCoefficients -> Size GTxWitnessed -> Fees
calcFeeTx FeeCoefficients{..} (Size size) =
    Fees $ Coin $ unCoin fcMinimal + round (fcMultiplier * fromIntegral size)

-- | Calculates tx fee based on the number of txs in the private block.
calcFeePub :: FeeCoefficients -> Word32 -> Fees
calcFeePub FeeCoefficients{..} txsN =
    Fees $ Coin $ unCoin fcMinimal + round (fcMultiplierPub * fromIntegral txsN)

-- | Calculates fees of 'GTxWitnessed'.
calcFeeG :: FeeCoefficients -> GTxWitnessed -> Fees
calcFeeG coeffs = \case
    tx@(GMoneyTxWitnessed _) ->
        calcFeeTx coeffs $ sizeSerialised tx
    (GPublicationTxWitnessed (PublicationTxWitnessed { ptwTx })) ->
        calcFeePub coeffs $ mrSize $ ptHeader ptwTx ^. pbhBodyProof

-- | "No fee" setup.
noFees :: FeeCoefficients
noFees = FeeCoefficients
    { fcMinimal       = Coin 0
    , fcMultiplier    = 0
    , fcMultiplierPub = 0
    }

-- | Pick fees which would fit for given transaction. Fits only the
-- case when transaction size grows monotonically with fees.
--
-- [DSCP-252]: this function would work only for linear fees policy,
-- for other policies another algorithm would be required.
fixFees :: FeeCoefficients -> (Fees -> TxWitnessed) -> TxWitnessed
fixFees coeffs buildTx = go (Fees $ fcMinimal coeffs)
  where
    go fees =
        let tx = buildTx fees
            fees' = calcFeeTx coeffs $ sizeSerialised (GMoneyTxWitnessed tx)
        in case fees `compare` fees' of
            EQ -> tx
            LT -> go fees'
            GT -> error "fixFees: fees unexpectedly decreased"
