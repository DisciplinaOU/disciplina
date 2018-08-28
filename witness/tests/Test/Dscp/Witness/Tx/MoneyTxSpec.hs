module Test.Dscp.Witness.Tx.MoneyTxSpec where

import Control.Lens (makeLenses, makeLensesWith, traversed)
import Data.List (scanl1)
import qualified GHC.Exts as Exts
import Test.QuickCheck (resize)
import Test.QuickCheck.Modifiers (NonNegative (..))
import Test.QuickCheck.Monadic (pre)

import Dscp.Core
import Dscp.Crypto
import Dscp.Util
import Dscp.Util.Test
import Dscp.Witness
import Test.Dscp.Witness.Mode

-- | Describes tx construction in steps. This is something you want
-- to vary in order to produce defective transactions.
-- If you change one of early steps in transaction construction,
-- later ones will come up (e.g., changing 'tcsTx' will not break signature).
data TxCreationSteps = TxCreationSteps
    { _tcsInAcc   :: Address -> TxInAcc
    , _tcsTx      :: TxInAcc -> Coin -> [TxOut] -> Tx
    , _tcsWitness :: Signature (TxId, PublicKey, ()) -> PublicKey -> TxWitness
    }

makeLenses ''TxCreationSteps

-- | Correct way of creating the first transaction.
properSteps :: TxCreationSteps
properSteps = TxCreationSteps
    { _tcsInAcc = \tiaAddr ->
        TxInAcc{ tiaNonce = 0, tiaAddr }
    , _tcsTx = \txInAcc txInValue txOuts' ->
        Tx{ txInAcc, txInValue, txOuts = txOuts' }
    , _tcsWitness = TxWitness
    }

-- | Exact values for transaction.
data TxData = TxData
    { tdSecret :: SecretKey
    , tdOuts   :: NonEmpty TxOut
    } deriving (Show, Generic)

makeLensesWith postfixLFields ''TxData

instance Arbitrary TxData where
    arbitrary = do
        tdSecret <- elements testGenesisSecrets
        tdOuts <- resize 10 arbitrary
        return TxData{..}

makeTx :: TxCreationSteps -> TxData -> TxWitnessed
makeTx steps dat =
    let sourcePk = toPublic (tdSecret dat)
        inAcc = _tcsInAcc steps (mkAddr sourcePk)
        spent = leftToPanic $ sumCoins $ map txOutValue $ toList (tdOuts dat)
        tx = _tcsTx steps inAcc spent (toList $ tdOuts dat)
        sgn = sign (tdSecret dat) (toTxId tx, sourcePk, ())
        witness = _tcsWitness steps sgn sourcePk
    in TxWitnessed{ twTx = tx, twWitness = witness }

makeTxsChain :: Int -> TxCreationSteps -> TxData -> NonEmpty TxWitnessed
makeTxsChain n steps dat
    | n <= 0 = error "makeTxsChain: n <= 0"
    | otherwise =
        Exts.fromList [0 .. fromIntegral n - 1] <&> \i ->
            let steps' = steps & tcsInAcc %~ \f addr -> (f addr){ tiaNonce = i }
            in makeTx steps' dat


applyTx :: WithinWriteSDLock => TxWitnessed -> WitnessTestMode ()
applyTx tw = void $ addTxToMempool (GMoneyTxWitnessed tw)

spec :: Spec
spec = describe "Money tx expansion + validation" $ do
    it "Correct tx is fine" $ witnessProperty_ $ do
        txData <- pick arbitrary
        let tx = makeTx properSteps txData
        lift $ applyTx tx

    describe "Transaction input part" $ do
        it "Bad nonce is not fine" $ witnessProperty $ do
            txData <- pick arbitrary
            nonce <- pick $ arbitrary `suchThat` (/= 0)
            let steps = properSteps & tcsInAcc .~ \addr -> TxInAcc addr nonce
            let tx = makeTx steps txData
            lift $ throwsSome $ applyTx tx

        it "Bad input address is not fine" $ witnessProperty $ do
            txData <- pick arbitrary
            sourceAddr <- pick (arbitrary @Address)
            pre (sourceAddr /= mkAddr (toPublic $ tdSecret txData))

            let steps = properSteps
                    & tcsInAcc %~ \f addr -> (f addr){ tiaAddr = sourceAddr }
            let tx = makeTx steps txData
            lift $ throwsSome $ applyTx tx

        it "Having not enough money is not fine" $ witnessProperty $ do
            txData' <- pick arbitrary
            secret <- pick $ arbitrary `suchThat` (/= tdSecret txData')
            let txData = txData'{ tdSecret = secret }

            let tx = makeTx properSteps txData
            lift $ throwsSome $ applyTx tx

        it "Too high input is processed fine" $ witnessProperty $ do
            txData <- pick arbitrary
            belowMax <- pick $ getNonNegative <$> resize 100000 arbitrary
            let spent = leftToPanic . coinFromInteger $
                        coinToInteger maxBound - belowMax

            let steps = properSteps
                    & tcsTx %~ \f inAcc _ outs -> f inAcc spent outs
            let tx = makeTx steps txData
            lift $ throwsSome $ applyTx tx

        it "Can't spend more money than currently present" $ witnessProperty $ do
            txOuts' <- pick infiniteList
            let manyEnough = succ $ length $
                             takeWhile (<= coinToInteger testGenesisAddressAmount) $
                             scanl1 (+) (map (coinToInteger . txOutValue) txOuts')
            let txOuts = Exts.fromList $ take manyEnough txOuts'
            txData <- pick $ arbitrary <&> \td -> td{ tdOuts = txOuts }
            let tx = makeTx properSteps txData
            lift $ throwsSome $ applyTx tx

    describe "Transaction output part" $ do
        it "Empty transaction output is not fine" $ witnessProperty $ do
            txData <- pick arbitrary
            let steps = properSteps
                    & tcsTx %~ \f inAcc inVal _ -> f inAcc inVal []
            let tx = makeTx steps txData
            lift $ throwsSome $ applyTx tx

        it "Large outputs are fine" $ witnessProperty $ do
            -- large input value was already tested above, so we ignore
            -- @inVal > sum outVals@ requirement here in order to prevent coin
            -- overflow
            txData' <- pick arbitrary
            let txData = txData' & tdOutsL . traversed . txOutValueL .~ maxBound @Coin
            let steps = properSteps
                    & tcsTx %~ \f inAcc _ outs -> f inAcc (maxBound @Coin) outs
            let tx = makeTx steps txData
            lift $ throwsSome $ applyTx tx

    describe "Transaction signature" $ do
        it "Changing tx parts without updating signature fails" $ witnessProperty $ do
            txData <- pick arbitrary
            let saneTw = makeTx properSteps txData
            fakeTw <- pick arbitrary
            mixTw <- pick $ arbitraryUniqueMixture saneTw fakeTw
            lift $ throwsSome $ applyTx mixTw

    describe "Overall sanity" $ do
        it "Tx input < sum tx outs + eps => failure" $ witnessProperty $ do
            txData <- pick arbitrary
            let spent = leftToPanic $ sumCoins $ map txOutValue $
                        toList (tdOuts txData)
                steps = properSteps & tcsTx %~
                    \f inAcc _ outs -> f inAcc spent outs
                tx = makeTx steps txData
            lift $ throwsSome $ applyTx tx

    describe "State modifications are correct" $ do
        it "Several transactions and invalid nonce" $ witnessProperty $ do
            txData <- pick arbitrary
            txNum <- pick $ choose (2, 10)
            let txs = makeTxsChain txNum properSteps txData
            -- going to skip transaction before the last one
            let initTxs = init . Exts.fromList $ init txs
            lift $ forM_ initTxs applyTx
            lift $ throwsSome $ applyTx (last txs)

        it "Several transactions exhausting account" $ witnessProperty $ do
            txData' <- pick arbitrary
            destination <- pick arbitrary
            let spentPerTx = leftToPanic . coinFromInteger $
                             (coinToInteger testGenesisAddressAmount * 3) `div` 4
                txData = txData'{ tdOuts = one $ TxOut destination spentPerTx }
                txs = makeTxsChain 2 properSteps txData
            lift $ throwsSome $ forM_ txs applyTx
