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
import Data.Default (def)
import Fmt ((+|), (|+))

import Dscp.Core
import Dscp.Snowdrop
import Dscp.Util (fromHex, nothingToThrow)
import Dscp.Util
import Dscp.Util.Concurrent.NotifyWait
import Dscp.Witness.Launcher.Mode (WitnessWorkMode)
import Dscp.Witness.Logic
import qualified Dscp.Witness.Relay as Relay
import Dscp.Witness.SDLock
import Dscp.Witness.Web.Error
import Dscp.Witness.Web.Types

----------------------------------------------------------------------------
-- Logic
----------------------------------------------------------------------------

-- | Applies transaction.
-- Once call of this function returns, transaction is put to mempool gets
-- considered by 'getAccountInfo' and other endpoints. It may be not send
-- into network yet.
submitUserTx :: WitnessWorkMode ctx m => TxWitnessed -> m ()
submitUserTx tw =
    Relay.relayTx (GMoneyTxWitnessed tw) >>= wait @"tx in mempool"

-- | Applies transaction, but does not wait transaction to appear in mempool.
submitUserTxAsync :: WitnessWorkMode ctx m => TxWitnessed -> m ()
submitUserTxAsync tw =
    void . Relay.relayTx $ GMoneyTxWitnessed tw

toBlockInfo :: WitnessWorkMode ctx m => Bool -> Block -> m BlockInfo
toBlockInfo includeTxs block = do
    nextHash <- runSdMRead $ resolveNext hh
    pure BlockInfo
        { biHeaderHash = hh
        , biNextHash = nextHash
        , biMerkleRootHash = "..." -- TODO
        , biHeader = bHeader block
        , biIsGenesis = block == genesisBlock
        , biSince = getSlotSince . hSlotId . bHeader $ block
        , biSize = BS.length . serialise $ block
        , biTransactionCount = length txs
        , biTotalOutput = leftToPanic . sumCoins $ txTotalOutput . unGTxWitnessed <$> txs
        , biTotalFees = Coin 0  -- TODO
        , biTransactions =
            if includeTxs
            then Just $ TxInfo Nothing . unGTxWitnessed <$> txs
            else Nothing
        }
  where
    hh = headerHash block
    txs = bbTxs . bBody $ block
    txTotalOutput (GMoneyTx tx)      = leftToPanic $ sumCoins $ txOutValue <$> txOuts tx
    txTotalOutput (GPublicationTx _) = Coin 0

toAccountInfo :: WitnessWorkMode ctx m => BlocksOrMempool Account -> Maybe [GTxInBlock] -> m AccountInfo
toAccountInfo account txs = do
    transactions <- mapM (mapM toTxInfo) txs
    pure AccountInfo
        { aiBalances = Coin . fromIntegral . aBalance <$> account
        , aiCurrentNonce = nonce
        , aiTransactionCount = fromIntegral nonce
        , aiTransactions = transactions
        }
  where
    nonce = aNonce $ bmTotal account

toTxInfo :: WitnessWorkMode ctx m => GTxInBlock -> m TxInfo
toTxInfo tx = do
    blockInfo <- mapM (toBlockInfo False) $ tbBlock tx
    pure TxInfo
        { tiBlock = blockInfo
        , tiTx = unGTxWitnessed $ tbTx tx
        }

getBlocks :: WitnessWorkMode ctx m => Maybe Word64 -> Maybe Int -> m BlockList
getBlocks mSkip mCount = do
    (eBlocks, totalCount) <- runSdMRead $ do
        eBlocks <- getBlocksFrom skip count
        totalCount <- (+ 1) . unDifficulty . hDifficulty <$> getTipHeader
        return (eBlocks, totalCount)
    either (throwM . InternalError) (toBlockList totalCount <=< mapM (toBlockInfo False) . reverse . toList) eBlocks
  where
    count = min 100 $ fromMaybe 100 mCount
    skip = fromMaybe 0 mSkip
    toBlockList blTotalCount blBlocks = pure BlockList{..}

getBlockInfo :: WitnessWorkMode ctx m => HeaderHash -> m BlockInfo
getBlockInfo hh =
    runSdMRead (getBlockMaybe hh) >>=
    nothingToThrow (LogicError $ LEBlockAbsent $ "Block " +| hh |+ " not found") >>=
    toBlockInfo True

getAccountInfo :: WitnessWorkMode ctx m => Address -> Bool -> m AccountInfo
getAccountInfo address includeTxs = do
    account <- readingSDLock $ do
        blockAccount <- fromMaybe def <$> getAccountMaybe address
        poolAccount  <- fromMaybe def <$> getMempoolAccountMaybe address
        return BlocksOrMempool
              { bmConfirmed = blockAccount
              , bmTotal     = poolAccount }
    txs <- if includeTxs
        then Just <$> getAccountTxs address
        else return Nothing
    toAccountInfo account txs

getTransactions :: WitnessWorkMode ctx m => Maybe Int -> Maybe GTxId -> m TxList
getTransactions mCount mFrom = do
    whenJust mFrom $ \from ->
        runSdMRead (getTxMaybe from) >>=
        void . nothingToThrow (LogicError . LETxAbsent $ "Transaction not found " +| from |+ "")
    eTxs <- runSdMRead $ getTxs (count + 1) mFrom
    either (throwM . InternalError) (toTxList . reverse . toList) eTxs
  where
    count = min 100 $ fromMaybe 100 mCount
    toTxList txs = do
        transactions <- mapM toTxInfo $ take count txs
        pure TxList
            { tlTransactions = transactions
            , tlNextId = map (toGTxId . unGTxWitnessed . tbTx) . safeHead $ drop count txs
            }

getTransactionInfo :: WitnessWorkMode ctx m => GTxId -> m TxInfo
getTransactionInfo txId =
    runSdMRead (getTxMaybe txId) >>=
    nothingToThrow (LogicError . LETxAbsent $ "Transaction not found " +| txId |+ "") >>=
    toTxInfo

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
        GMoneyTx _ -> HashIsMoneyTx
        GPublicationTx _ -> HashIsPublicationTx
