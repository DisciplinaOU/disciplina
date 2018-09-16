module Test.Dscp.Witness.FeeSpec where

import Dscp.Core
import Dscp.Crypto
import Dscp.Util.Test

-- | There is a little sense to try all possible coefficients since this will
-- hardly reveal edge cases processed wrong.
-- TODO: get from config ([DSCP-90])?
testFeeCoefficients :: FeeCoefficients
testFeeCoefficients =
    FeeCoefficients
        {fcMinimal = Coin 10, fcMultiplier = 0.1, fcMinimalPub = Coin 10, fcMultiplierPub = 0.1}

spec :: Spec
spec = describe "Transaction fees" $ do
    it "fixFees always converges for money tx" . property $ \txTemplate sk ->
        let tw = fixFees testFeeCoefficients $ \fees ->
                let outs = txOuts txTemplate
                    inValue = Coin (sum $ map (unCoin . txOutValue) outs)
                              `unsafeAddCoin` unFees fees
                    tx = txTemplate{ txInValue = inValue }

                    pk = toPublic sk
                    signature   = sign sk (toTxId tx, pk, ())
                    witness     = TxWitness   { txwSig = signature, txwPk = pk }
                    txWitnessed = TxWitnessed { twTx   = tx, twWitness = witness }
                in txWitnessed
        in tw `seq` ()
