module Dscp.Witness.Web.Logic
       ( submitUserTx
       , submitUserTxAsync
       , getBlocks
       , getBlockInfo
       , getAccountInfo
       , getTransactions
       , getTransactionInfo
       ) where

import UnliftIO.Async (async)

import Dscp.Core
import Dscp.Snowdrop
import Dscp.Util (assertJust, nothingToThrow)
import Dscp.Witness.Launcher.Mode (WitnessWorkMode)
import qualified Dscp.Witness.Relay as Relay
import Dscp.Witness.Config
import Dscp.Witness.Logic
import Dscp.Witness.Web.Error
import Dscp.Witness.Web.Types

----------------------------------------------------------------------------
-- Logic
----------------------------------------------------------------------------

-- | Applies transaction everywhere.
submitUserTx :: WitnessWorkMode ctx m => TxWitnessed -> m ()
submitUserTx = Relay.relayTx . GMoneyTxWitnessed

-- | Applies transaction, but does not wait for a whole cycle of transaction
-- application.
submitUserTxAsync :: WitnessWorkMode ctx m => TxWitnessed -> m ()
submitUserTxAsync tw = void . async $ submitUserTx tw

toBlockInfo :: HasWitnessConfig => Bool -> Block -> BlockInfo
toBlockInfo includeTxs block = BlockInfo
    { biHeaderHash = hh
    , biHeader = bHeader block
    , biIsGenesis = block == genesisBlock
    , biTransactions =
        if includeTxs
        then Just . map (TxInfo (Just hh) . unGTxWitnessed) . bbTxs . bBody $ block
        else Nothing
    }
  where
    hh = headerHash block

toAccountInfo :: Account -> Maybe [GTxInBlock] -> AccountInfo
toAccountInfo account txs = AccountInfo
    { aiBalances = Balances
        { bConfirmed = Coin . fromIntegral $ aBalance account
        }
    , aiNextNonce = aNonce account + 1
    , aiTransactions = map toTxInfo <$> txs
    }

toTxInfo :: GTxInBlock -> TxInfo
toTxInfo tx = TxInfo
    { tiHeaderHash = tbHeaderHash tx
    , tiTx = unGTxWitnessed $ tbTx tx
    }

getBlocks :: WitnessWorkMode ctx m => Maybe Int -> Maybe HeaderHash -> m [BlockInfo]
getBlocks mCount mFrom = do
    let count = max 100 $ fromMaybe 100 mCount
    from <- maybe (runSdMRead getTipHash) return mFrom
    runSdMRead (getBlockMaybe from) >>= void . nothingToThrow BlockNotFound
    eBlocks <- runSdMRead $ getBlocksBefore count from
    either (throwM . InternalError) (return . map (toBlockInfo False) . toList) eBlocks

getBlockInfo :: WitnessWorkMode ctx m => HeaderHash -> m BlockInfo
getBlockInfo =
    runSdMRead . getBlockMaybe >=>
    nothingToThrow BlockNotFound >=>
    return . toBlockInfo True

getAccountInfo :: WitnessWorkMode ctx m => Address -> Bool -> m AccountInfo
getAccountInfo address includeTxs = do
    account <- getAccountMaybe address `assertJust` AccountNotFound
    txs <- if includeTxs
        then Just <$> getAccountTxs address
        else return Nothing
    return $ toAccountInfo account txs

getTransactions :: WitnessWorkMode ctx m => Maybe Int -> m [TxInfo]
getTransactions mCount = do
    let count = max 100 $ fromMaybe 100 mCount
    eTxs <- runSdMRead $ getTxs count
    return . map toTxInfo . toList $ eTxs

getTransactionInfo :: WitnessWorkMode ctx m => GTxId -> m TxInfo
getTransactionInfo =
    runSdMRead . getTxMaybe >=>
    nothingToThrow TransactionNotFound >=>
    return . toTxInfo
