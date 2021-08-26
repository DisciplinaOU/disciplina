module Dscp.TxPerf.Tx where

import Data.List (delete)
import Test.QuickCheck

import Dscp.Core
import Dscp.TxPerf.Account
import Dscp.Util
import Dscp.Witness.Logic.Tx
import Dscp.Witness.Web

-- | :(
moneyFeeConfig :: FeePolicy Tx
moneyFeeConfig =
    LinearFeePolicy FeeCoefficients
    { fcMinimal = Coin 1, fcMultiplier = 1 }

sendTx :: WitnessClient -> Bool -> Account -> [(Account, Coin)] -> IO Tx
sendTx wc async from tos = do
    void $ (if async then wSubmitTxAsync else wSubmitTx) wc txWitnessed
    print . toHex . toTxId $ tx
    print . pretty $ tx
    return tx
  where
    outs = (\(acc, value) -> TxOut (skAddress $ sk acc) value) <$> tos
    txWitnessed = createTxw moneyFeeConfig (sk from) (currentNonce from) outs
    tx = twTx txWitnessed

genTx :: WitnessClient -> Bool -> StateT [Account] IO ()
genTx wc async = do
    accounts <- get

    accFrom <- liftIO . generate $ elements accounts `suchThat` accHasMoney
    let toCount = length accounts `min` balance accFrom
    accsTo <- liftIO . generate . resize toCount . listOf1 $ elements $ delete accFrom accounts

    liftIO $
        void (sendTx wc async accFrom (map (, Coin 1) . ordNub $ accsTo))
        `catch` \(e :: WitnessApiClientError) ->
                    putStrLn $ "Tx sending error: " <> pretty e

    let upd =
            (\acc -> if acc == accFrom
                then acc
                    { balance = balance acc - toCount
                    , currentNonce = currentNonce acc + 1
                    }
                else acc
            ) .
            (\acc -> if acc `elem` accsTo
                then acc { balance = balance acc + 1 }
                else acc
            )
    put $ map upd accounts
