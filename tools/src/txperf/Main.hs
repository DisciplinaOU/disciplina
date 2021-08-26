import Dscp.TxPerf.Account
import Dscp.TxPerf.Params
import Dscp.TxPerf.Tx
import Dscp.Witness.Web

main :: IO ()
main = do
    Params{..} <- getCLIParams
    wc <- createWitnessClient paramWitness

    keyAccs <- forM paramAccKeys $ skToAcc wc
    genAccs <- sequence . take paramGenAccs . repeat $ genAcc wc
    let accounts = keyAccs ++ genAccs

    unless (any accHasMoney accounts) $ fail "No money to send around"

    flip evalStateT accounts $
        forM_ [1..paramTxCount] $ const $ genTx wc paramTxAsync
