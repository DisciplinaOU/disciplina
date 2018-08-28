module Test.Dscp.Witness.Tx.MoneyTxSpec where

import Control.Lens (makeLenses, makeLensesWith, traversed)
import qualified Data.List as L
import qualified Data.Text.Buildable
import Fmt (listF, (+|), (|+))
import qualified GHC.Exts as Exts
import Test.QuickCheck.Monadic (pre)
import qualified Text.Show

import Dscp.Core
import Dscp.Crypto
import Dscp.Snowdrop
import Dscp.Util
import Dscp.Util.Test
import Dscp.Witness

import Test.Dscp.Witness.Common
import Test.Dscp.Witness.Mode

-- | Describes tx construction in steps. This is something you want
-- to vary in order to produce defective transactions.
-- If you change one of early steps in transaction construction,
-- later ones will come up (e.g., changing 'tcsTx' will not break signature
-- on itself).
data TxCreationSteps = TxCreationSteps
    { _tcsInAcc   :: Address -> TxInAcc
    , _tcsTx      :: TxInAcc -> Coin -> [TxOut] -> Tx
    , _tcsWitness :: Signature (TxId, PublicKey, ()) -> PublicKey -> TxWitness
    , _tcsFixFees :: (Fees -> TxWitnessed) -> TxWitnessed
    }

makeLenses ''TxCreationSteps

-- | Correct way of creating the first transaction.
properSteps :: HasWitnessConfig => TxCreationSteps
properSteps = TxCreationSteps
    { _tcsInAcc = \tiaAddr ->
        TxInAcc{ tiaNonce = 0, tiaAddr }
    , _tcsTx = \txInAcc txInValue txOuts' ->
        Tx{ txInAcc, txInValue, txOuts = txOuts' }
    , _tcsWitness = TxWitness
    , _tcsFixFees = fixFees (fcMoney feeConfig)
    }

-- | Exact values for transaction.
data TxData = TxData
    { tdSecret :: SecretKey
    , tdOuts   :: NonEmpty TxOut
    } deriving (Generic)

makeLensesWith postfixLFields ''TxData

instance Buildable TxData where
    build TxData{..} =
        "Secret of " +| mkAddr (toPublic tdSecret) |+
        ", outs: " +| listF tdOuts |+ ""

instance Show TxData where
    show = toString . pretty

-- | Generate `TxData` which does not make transaction invalid.
genSafeTxData :: Gen TxData
genSafeTxData = do
    tdSecret <- elements testGenesisSecrets
    tdOuts <- genSafeTxOuts 100 (choose (1, 5)) `suchThatMap` nonEmpty
    return TxData{..}

-- | Given creation steps, build a transaction.
makeTx :: HasWitnessConfig => TxCreationSteps -> TxData -> TxWitnessed
makeTx steps dat = _tcsFixFees steps $ \fee ->
    let sourcePk = toPublic (tdSecret dat)
        inAcc = _tcsInAcc steps (mkAddr sourcePk)
        spent = leftToPanic $ sumCoins $
                unFees fee : (map txOutValue $ toList (tdOuts dat))
        tx = _tcsTx steps inAcc spent (toList $ tdOuts dat)
        sgn = sign (tdSecret dat) (toTxId tx, sourcePk, ())
        witness = _tcsWitness steps sgn sourcePk
    in TxWitnessed{ twTx = tx, twWitness = witness }

-- | Given creation steps, build several similar transactions.
makeTxsChain
    :: HasWitnessConfig
    => Int -> TxCreationSteps -> TxData -> NonEmpty TxWitnessed
makeTxsChain n steps dat
    | n <= 0 = error "makeTxsChain: n <= 0"
    | otherwise =
        Exts.fromList [0 .. fromIntegral n - 1] <&> \i ->
            let steps' = steps & tcsInAcc %~ \f addr -> (f addr){ tiaNonce = i }
            in makeTx steps' dat

-- | Submit transaction to validation.
applyTx :: (HasWitnessConfig, WithinWriteSDLock) => TxWitnessed -> WitnessTestMode' ()
applyTx tw = void $ addTxToMempool (GMoneyTxWitnessed tw)

spec :: Spec
spec = describe "Money tx expansion + validation" $ do
    it "Correct tx is fine" $ witnessProperty $ do
        txData <- pick genSafeTxData
        let tx = makeTx properSteps txData
        lift . noThrow $ applyTx tx

    describe "Transaction input part" $ do
        it "Bad nonce is not fine" $ witnessProperty $ do
            txData <- pick genSafeTxData
            nonce <- pick $ arbitrary `suchThat` (/= 0)
            let steps = properSteps & tcsInAcc .~ \addr -> TxInAcc addr nonce
            let tx = makeTx steps txData
            lift $ throwsPrism (_AccountError . _NonceMustBeIncremented) $
                applyTx tx

        it "Bad input address is not fine" $ witnessProperty $ do
            txData <- pick genSafeTxData
            sourceAddr <- pick (arbitrary @Address)
            pre (sourceAddr /= mkAddr (toPublic $ tdSecret txData))

            let steps = properSteps
                    & tcsInAcc %~ \f addr -> (f addr){ tiaAddr = sourceAddr }
            let tx = makeTx steps txData
            lift $ throwsSome $ applyTx tx

        it "Having no money is not fine" $ witnessProperty $ do
            txData' <- pick genSafeTxData
            secret <- pick $ arbitrary `suchThat` (`notElem` testGenesisSecrets)
            let txData = txData'{ tdSecret = secret }

            let tx = makeTx properSteps txData
            lift $ throwsPrism (_AccountError . _BalanceCannotBecomeNegative) $
                applyTx tx

        it "Can't spend more money than currently present" $ witnessProperty $ do
            txOuts' <- pick $ genSafeTxOuts (unCoin testGenesisAddressAmount `div` 2)
                                            (pure 10)
            let isEnough = (>= coinToInteger testGenesisAddressAmount)
            let manyEnoughL = map fst . dropWhile (not . isEnough . snd) $
                              zip (inits txOuts') $
                              L.scanl (+) 0 (fmap (coinToInteger . txOutValue) txOuts')
            pre (not $ null manyEnoughL)
            let txOuts = L.head $ manyEnoughL
            txData <- pick $ genSafeTxData <&> \td -> td{ tdOuts = Exts.fromList txOuts }
            let tx = makeTx properSteps txData
            lift $ throwsPrism (_AccountError . _BalanceCannotBecomeNegative) $
                applyTx tx

        it "Too high input is processed fine" $ witnessProperty $ do
            txData <- pick genSafeTxData <&> tdOutsL %~ one . head
            belowMax <- pick $ choose (0, coinToInteger testGenesisAddressAmount `div` 2)
            let spent = leftToPanic . coinFromInteger $
                        coinToInteger testGenesisAddressAmount - belowMax

            let steps = properSteps
                    & tcsTx %~ \f inAcc _ outs -> f inAcc spent outs
            let tx = makeTx steps txData
            lift . noThrow $ applyTx tx

    describe "Transaction output part" $ do
        it "Empty transaction output is not fine" $ witnessProperty $ do
            txData <- pick genSafeTxData
            let steps = properSteps
                    & tcsTx %~ \f inAcc inVal _ -> f inAcc inVal []
            let tx = makeTx steps txData
            lift $ throwsPrism (_AccountError . _MTxNoOutputs) $
                applyTx tx

        it "Non positive output amount is not fine" $ witnessProperty $ do
            txData <- pick genSafeTxData
                      <&> tdOutsL . _headNE . txOutValueL .~ minBound @Coin
            let tx = makeTx properSteps txData
            lift $ throwsPrism (_AccountError .
                                (_ReceiverMustIncreaseBalance <> _PaymentMustBePositive)) $
                applyTx tx

        it "Large outputs are fine" $ witnessProperty $ do
            -- large input value was already tested above, so we ignore
            -- @inVal > sum outVals@ requirement here in order to prevent coin
            -- overflow
            txData' <- pick genSafeTxData
            let txData = txData' & tdOutsL . traversed . txOutValueL .~ maxBound @Coin
            let steps = properSteps
                    & tcsTx %~ \f inAcc _ outs -> f inAcc (maxBound @Coin) outs
            let tx = makeTx steps txData
            lift $ throwsSome $ applyTx tx

    describe "Transaction signature" $ do
        it "Changing tx parts without updating signature fails" $ witnessProperty $ do
            txData <- pick genSafeTxData
            let saneTw = makeTx properSteps txData
            fakeTw <- pick arbitrary
            mixTw <- pick $ arbitraryUniqueMixture saneTw fakeTw
            lift $ throwsSome $ applyTx mixTw

    describe "Overall sanity" $ do
        it "Tx input < sum tx outs => failure" $ witnessProperty $ do
            txData <- pick genSafeTxData
            let Coin outSum = leftToPanic $ sumCoins $ map txOutValue $
                              toList (tdOuts txData)
            pre (outSum > 1)
            spent <- pick $ Coin <$> choose (1, outSum - 1)

            let steps = properSteps & tcsTx %~
                    \f inAcc _ outs -> f inAcc spent outs
                tx = makeTx steps txData
            lift $ throwsPrism (_AccountError . _InsufficientFees) $
                applyTx tx

        it "Tx input < sum tx outs + fee => failure" $ witnessProperty $ do
            txData <- pick genSafeTxData
            let Coin outSum = leftToPanic $ sumCoins $ map txOutValue $
                              toList (tdOuts txData)
            Coin minFee <- case fcMoney feeConfig of
                LinearFeePolicy FeeCoefficients{..} -> pure fcMinimal
            inVal <- pick $ choose (outSum, outSum + minFee - 1)

            let steps = properSteps & tcsTx %~
                    \f inAcc _ outs -> f inAcc (Coin inVal) outs
                tx = makeTx steps txData
            lift $ throwsPrism (_AccountError . _InsufficientFees) $
                applyTx tx

    describe "State modifications are correct" $ do
        it "Several transactions and invalid nonce" $ witnessProperty $ do
            txData <- pick genSafeTxData
            txNum <- pick $ choose (2, 10)
            let txs = makeTxsChain txNum properSteps txData
            -- going to skip transaction before the last one
            let initTxs = init . Exts.fromList $ init txs
            lift $ forM_ initTxs applyTx
            lift $ throwsPrism (_AccountError . _NonceMustBeIncremented) $
                applyTx (last txs)

        it "Several transactions exhausting account" $ witnessProperty $ do
            txData' <- pick genSafeTxData
            destination <- pick arbitrary
            let spentPerTx = leftToPanic . coinFromInteger $
                             (coinToInteger testGenesisAddressAmount * 3) `div` 4
                txData = txData'{ tdOuts = one $ TxOut destination spentPerTx }
                txs = makeTxsChain 2 properSteps txData
            lift $ throwsPrism (_AccountError . _BalanceCannotBecomeNegative) $
                mapM_ applyTx txs
