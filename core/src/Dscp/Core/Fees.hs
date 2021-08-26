{-# LANGUAGE GADTs            #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData       #-}
{-# LANGUAGE TypeOperators    #-}

-- | Trnsaction fees processing and calculation.
module Dscp.Core.Fees
       ( Fees (..)
       , _Fees
       , FeeCoefficients (..)
       , FeePolicy (..)
       , FeeConfig
       , FeeConfigRecP
       , FeeConfigRec
       , calcLinearFees
       , calcFee
       , calcFeePub
       , calcFeeG
       , noFees
       , noFeesConfig
       , fixFees
       , estimateFees
       ) where

import Codec.Serialise (Serialise (..))
import Control.Lens (makePrisms, (?~))
import Loot.Config ((:::), Config, PartialConfig, finaliseDeferredUnsafe, option)

import Dscp.Core.Foundation
import Dscp.Crypto
import Dscp.Util

-- | Amount of transaction fees.
newtype Fees = Fees { unFees :: Coin }
    deriving (Eq, Ord, Show, Generic, Hashable, Bounded)

makePrisms ''Fees

instance Serialise Fees

-- | Setup for fee calculation by linear rule.
data FeeCoefficients = FeeCoefficients
    { fcMinimal    :: Coin  -- ^ How much is the base fee?
    , fcMultiplier :: Float -- ^ How much tokens taken per item?
    } deriving (Eq, Show)

-- | General setup for fee calculation
data FeePolicy tx where
    LinearFeePolicy :: FeeCoefficients -> FeePolicy tx
    UnknownFeePolicy :: FeePolicy Void

deriving instance Eq (FeePolicy tx)
deriving instance Show (FeePolicy tx)

-- | All fee policies.
type FeeConfig =
    '[ "money"       ::: FeePolicy Tx
     , "publication" ::: FeePolicy PublicationTx
     ]

type FeeConfigRecP = PartialConfig FeeConfig
type FeeConfigRec = Config FeeConfig

-- | Calculate fees from tx size.
calcLinearFees :: Integral i => FeeCoefficients -> i -> Fees
calcLinearFees FeeCoefficients{..} size =
    Fees $ Coin $ unCoin fcMinimal + round (fcMultiplier * fromIntegral size)

-- | Calculates fees of money transaction.
calcFee :: FeePolicy Tx -> TxWitnessed -> Fees
calcFee policy tx = case policy of
    LinearFeePolicy coeffs ->
        calcLinearFees coeffs $ unSize $ sizeSerialised (GMoneyTxWitnessed tx)

-- | Calculates fees of publication transaction.
calcFeePub :: FeePolicy PublicationTx -> PrivateBlockHeader -> Fees
calcFeePub policy header = case policy of
    LinearFeePolicy coeffs ->
        calcLinearFees coeffs $ msSize $ header ^. pbhBodyProof

-- | Calculates fees of 'GTxWitnessed'.
calcFeeG :: FeeConfigRec -> GTxWitnessed -> Fees
calcFeeG feeConfig = \case
    (GMoneyTxWitnessed tw) ->
        calcFee (feeConfig ^. option #money) tw
    (GPublicationTxWitnessed PublicationTxWitnessed { ptwTx }) ->
        calcFeePub (feeConfig ^. option #publication) (ptHeader ptwTx)

-- | "No fee" setup.
noFees :: FeePolicy tx
noFees = LinearFeePolicy FeeCoefficients
    { fcMinimal       = Coin 0
    , fcMultiplier    = 0
    }

noFeesConfig :: FeeConfigRec
noFeesConfig = finaliseDeferredUnsafe $ mempty
    & option #money       ?~ noFees
    & option #publication ?~ noFees

-- | Pick fees which would fit for given transaction. Fits only the
-- case when transaction size grows monotonically with fees.
fixLinearFees :: FeeCoefficients -> (Fees -> TxWitnessed) -> TxWitnessed
fixLinearFees coeffs buildTx = go (Fees $ fcMinimal coeffs)
  where
    go fees =
        let tx = buildTx fees
            Size size = sizeSerialised (GMoneyTxWitnessed tx)
            fees' = calcLinearFees coeffs size
        in case fees `compare` fees' of
            EQ -> tx
            LT -> go fees'
            GT -> error "fixLinearFees: fees unexpectedly decreased"

-- | Pick fees which would fit for given transaction.
fixFees :: FeePolicy Tx -> (Fees -> TxWitnessed) -> TxWitnessed
fixFees feePolicy = case feePolicy of
    LinearFeePolicy coeffs -> fixLinearFees coeffs

-- | Estimate fees for given outputs.
-- Only works for fixed nonce, key and signature sizes.
estimateFees :: FeeConfigRec -> [TxOut] -> Fees
estimateFees feeConfig outs = calcFeeG feeConfig gTx
  where
    publicKey = leftToPanic $ fromByteArray ("patakbardaq_skovoroda_pvaforever" :: ByteString)
    signature = leftToPanic $ fromByteArray ("patakbardaq_skovoroda_pvaforever_and_thirty_two_more_bytes_mkay?" :: ByteString)
    address = mkAddr publicKey

    gTx = GMoneyTxWitnessed . fixFees (feeConfig ^. option #money) $ \fees ->
        let inAcc = TxInAcc { tiaNonce = 0, tiaAddr = address }
            inValue = Coin (sum $ map (unCoin . txOutValue) outs) `unsafeAddCoin` unFees fees
            tx = Tx { txInAcc = inAcc, txInValue = inValue, txOuts = outs }
            txWitness = TxWitness { txwSig = signature, txwPk = publicKey }
            txWitnessed = TxWitnessed { twTx   = tx, twWitness = txWitness }
        in txWitnessed
