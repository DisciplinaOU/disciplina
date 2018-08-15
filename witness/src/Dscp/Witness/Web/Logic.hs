module Dscp.Witness.Web.Logic
       ( submitUserTx
       , submitUserTxAsync
       , getBlocks
       , getBlockInfo
       , getAccountInfo
       , getTransactions
       , getTransactionInfo
       , getHashType
       ) where

import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BS
import UnliftIO.Async (async)

import Dscp.Core
import Dscp.Snowdrop
import Dscp.Util (assertJust, fromHex, nothingToThrow)
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
    , biNextHash = hh  -- TODO
    , biMerkleRootHash = "..." -- TODO
    , biHeader = bHeader block
    , biIsGenesis = block == genesisBlock
    , biSince = getSlotSince . hSlotId . bHeader $ block
    , biSize = BS.length . serialise $ block
    , biTransactionCount = length txs
    , biTotalOutput = Coin $ sum $ unCoin . txTotalOutput . unGTxWitnessed <$> txs
    , biTotalFees = Coin 0  -- TODO
    , biTransactions =
        if includeTxs
        then Just $ TxInfo Nothing . unGTxWitnessed <$> txs
        else Nothing
    }
  where
    hh = headerHash block
    txs = bbTxs . bBody $ block
    txTotalOutput (GMoneyTx tx) = Coin $ sum $ unCoin . txOutValue <$> txOuts tx
    txTotalOutput (GPublicationTx _) = Coin 0

toAccountInfo :: HasWitnessConfig => Account -> Maybe [GTxInBlock] -> AccountInfo
toAccountInfo account txs = AccountInfo
    { aiBalances = Balances
        { bConfirmed = Coin . fromIntegral $ aBalance account
        }
    , aiNextNonce = aNonce account + 1
    , aiTransactionCount = aNonce account
    , aiTransactions = map toTxInfo <$> txs
    }

toTxInfo :: HasWitnessConfig => GTxInBlock -> TxInfo
toTxInfo tx = TxInfo
    { tiBlock = toBlockInfo False <$> tbBlock tx
    , tiTx = unGTxWitnessed $ tbTx tx
    }

getBlocks :: WitnessWorkMode ctx m => Maybe Int -> Maybe HeaderHash -> m [BlockInfo]
getBlocks mCount mFrom = do
    let count = min 100 $ fromMaybe 100 mCount
    from <- maybe (runSdMRead getTipHash) return mFrom
    runSdMRead (getBlockMaybe from) >>= void . nothingToThrow BlockNotFound
    eBlocks <- runSdMRead $ getBlocksBefore count from
    either (throwM . InternalError) (return . map (toBlockInfo False) . reverse . toList) eBlocks

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

getTransactions :: WitnessWorkMode ctx m => Maybe Int -> Maybe GTxId -> m TxList
getTransactions mCount mFrom = do
    whenJust mFrom $
        runSdMRead . getTxMaybe >=> void . nothingToThrow TransactionNotFound
    eTxs <- runSdMRead $ getTxs (count + 1) mFrom
    either (throwM . InternalError) (return . toTxList . reverse . toList) eTxs
  where
    count = min 100 $ fromMaybe 100 mCount
    toTxList txs = TxList
        { tlTransactions = toTxInfo <$> take count txs
        , tlNextId = map (toGTxId . unGTxWitnessed . tbTx) . safeHead $ drop count txs
        }

getTransactionInfo :: WitnessWorkMode ctx m => GTxId -> m TxInfo
getTransactionInfo =
    runSdMRead . getTxMaybe >=>
    nothingToThrow TransactionNotFound >=>
    return . toTxInfo

-- | As we can't distinguish between different hashes, we have to check whether an entity exists.
getHashType :: WitnessWorkMode ctx m => Text -> m HashIs
getHashType hash = fmap (fromMaybe HashIsUnknown) . runMaybeT . asum $
    [isBlock, isAddress, isTx] <*> [hash]
  where
    is hashType decode getMaybe =
        MaybeT . return . rightToMaybe . decode >=>
        MaybeT . getMaybe >=>
        MaybeT . return . Just . hashType
    isBlock = is
        (const HashIsBlock)
        (fromHex @HeaderHash)
        (runSdMRead . getBlockMaybe)
    isAddress = is
        (const HashIsAddress)
        addrFromText
        getAccountMaybe
    isTx = is
        (distinguishTx . unGTxWitnessed . tbTx)
        fromHex
        (runSdMRead . getTxMaybe . GTxId)
    distinguishTx = \case
        GMoneyTx _ -> HashIsTx
        GPublicationTx _ -> HashIsPublicationTx
