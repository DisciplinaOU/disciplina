module Dscp.TxPerf.Tx where

import Data.List (delete)
import Test.QuickCheck

import Dscp.Core
import Dscp.Crypto
import Dscp.TxPerf.Account
import Dscp.Util
import Dscp.Witness.Web

sendTx :: WitnessClient -> Bool -> Account -> [(Account, Coin)] -> IO Tx
sendTx wc async from tos = do
    (if async then wSubmitTxAsync else wSubmitTx) wc txWitnessed
    print . toHex . toTxId $ tx
    print . pretty $ tx
    return tx
  where
    inAcc = TxInAcc{ tiaNonce = nextNonce from, tiaAddr = addr from }
    inValue = foldr sumCoins (Coin 0) $ snd <$> tos
    outs = (\(acc, value) -> TxOut (addr acc) value) <$> tos
    tx = Tx{ txInAcc = inAcc, txInValue = inValue, txOuts = outs }

    signature = sign (sk from) (toTxId tx, pk from, ())
    witness = TxWitness{ txwSig = signature, txwPk = pk from }
    txWitnessed = TxWitnessed{ twTx = tx, twWitness = witness }

genTx :: WitnessClient -> Bool -> StateT [Account] IO ()
genTx wc async = do
    accounts <- get

    accFrom <- liftIO . generate $ elements accounts `suchThat` accHasMoney
    let toCount = length accounts `min` balance accFrom
    accsTo <- liftIO . generate . resize toCount . listOf1 $ elements $ delete accFrom accounts

    void . liftIO $ sendTx wc async accFrom (map (, Coin 1) . ordNub $ accsTo)

    let upd =
            (\acc -> if acc == accFrom
                then acc
                    { balance = balance acc - toCount
                    , nextNonce = nextNonce acc + 1
                    }
                else acc
            ) .
            (\acc -> if acc `elem` accsTo
                then acc{ balance = balance acc + 1 }
                else acc
            )
    put $ map upd accounts
