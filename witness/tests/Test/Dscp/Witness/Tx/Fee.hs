module Test.Dscp.Witness.Tx.Fee where

import Dscp.Core
import Dscp.Util.Test
import Dscp.Witness

-- | There is a little sense to try all possible coefficients since this will
-- hardly reveal edge cases processed wrong.
-- TODO: get from config ([DSCP-90])?
testFeeCoefficients :: FeeCoefficients
testFeeCoefficients =
    FeeCoefficients{fcMinimal = Coin 10, fcMultiplier = 0.1}

spec_Transaction_fees :: Spec
spec_Transaction_fees = do
    it "fixFees always converges for money tx and linear coefs" . property $
      \outs sk ->
        let tw = fixFees (LinearFeePolicy testFeeCoefficients) $ \fees ->
                   signTx sk $ createTx sk 0 outs fees
        in tw `seq` ()
