module Dscp.Witness.Web.Logic
       ( submitUserTx
       , submitUserTxAsync
       , getBlocks
       , getBlockInfo
       , getAccountInfo
       , getTransactions
       , getTransactionInfo
       , getPublications
       , getHashType
       ) where

import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BS
import Data.Coerce (coerce)
import Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.Default (def)
import Fmt ((+|), (|+))

import Dscp.Core
import Dscp.Snowdrop
import Dscp.Util
import Dscp.Util.Concurrent.NotifyWait
import Dscp.Witness.Config
import Dscp.Witness.Launcher.Context (WitnessWorkMode)
import Dscp.Witness.Logic
import Dscp.Witness.Mempool
import qualified Dscp.Witness.Relay as Relay
import Dscp.Witness.SDLock
import Dscp.Witness.Web.Types
import qualified Snowdrop.Util as SD

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

toBlockInfo
    :: HasWitnessConfig
    => Bool -> Block -> SdM_ chgacc BlockInfo
toBlockInfo includeTxs block = do
    nextHash <- resolveNext hh
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
            then Just $ WithBlockInfo Nothing . unGTxWitnessed <$> txs
            else Nothing
        }
  where
    hh = headerHash block
    txs = bbTxs . bBody $ block
    txTotalOutput (GMoneyTx tx)      = leftToPanic $ sumCoins $ txOutValue <$> txOuts tx
    txTotalOutput (GPublicationTx _) = Coin 0

toAccountInfo
    :: HasWitnessConfig
    => BlocksOrMempool Account
    -> Maybe [WithBlock GTxWitnessed]
    -> SdM_ chgacc AccountInfo
toAccountInfo account txs = do
    transactions <- mapM (mapM (fetchBlockInfo . fmap @WithBlock unGTxWitnessed)) txs
    pure AccountInfo
        { aiBalances = leftToPanic . coinFromInteger . aBalance <$> account
        , aiCurrentNonce = nonce
        , aiTransactionCount = fromIntegral nonce
        , aiTransactions = transactions
        }
  where
    nonce = aNonce $ bmTotal account

fetchBlockInfo
    :: HasWitnessConfig
    => WithBlock a -> SdM_ chgacc (WithBlockInfo a)
fetchBlockInfo txWithBlock = do
    blockInfo <- mapM (toBlockInfo False) $ wbBlock txWithBlock
    pure WithBlockInfo
        { wbiBlockInfo = blockInfo
        , wbiItem = wbItem txWithBlock
        }

getBlocks :: WitnessWorkMode ctx m => Maybe Word64 -> Maybe Int -> m BlockList
getBlocks mSkip mCount = runSdMLocked $ do
    blocks <- either (SD.throwLocalError . LEBlockAbsent) pure =<<
               getBlocksFrom skip count
    totalCount <- (+ 1) . unDifficulty . hDifficulty <$> getTipHeader
    toBlockList totalCount =<< mapM (toBlockInfo False) (reverse $ toList blocks)
  where
    count = min 100 $ fromMaybe 100 mCount
    skip = fromMaybe 0 mSkip
    toBlockList blTotalCount blBlocks = pure BlockList{..}

getBlockInfo
    :: WitnessWorkMode ctx m
    => HeaderHash -> m BlockInfo
getBlockInfo hh = runSdMLocked $
    getBlockMaybe hh >>=
    nothingToLocalError (LEBlockAbsent $ "Block " +| hh |+ " not found") >>=
    toBlockInfo True

getAccountInfo :: WitnessWorkMode ctx m => Address -> Bool -> m AccountInfo
getAccountInfo address includeTxs = do
    account <- readingSDLock $ do
        blockAccount <- runSdReadM $ fromMaybe def <$> getAccountMaybe address
        poolAccount  <- runSdReadM $ fromMaybe def <$> getMempoolAccountMaybe address
        return BlocksOrMempool
              { bmConfirmed = blockAccount
              , bmTotal     = poolAccount }
    -- TODO: remove this, same info can be found in an another place
    txs <- if includeTxs
        then runSdMLocked $ Just <$> getAccountTxs address
        else return Nothing
    runSdMLocked $ toAccountInfo account txs

toPaginatedList :: (Show a, Show (Id a)) => HasId a => Int -> [a] -> PaginatedList d a
toPaginatedList count allItems =
    let items = take count allItems
    in PaginatedList
        { plItems = items
        , plNextId = map getId . safeHead $ drop count allItems
        }

sinkTruncate
    :: (Monad m, HasId a, Show a, Show (Id a))
    => Int -> C.ConduitT a Void m (PaginatedList d a)
sinkTruncate count = C.take (count + 1) .| (toPaginatedList count <$> C.sinkList)

getTransactions
    :: (WitnessWorkMode ctx m)
    => Maybe Int -> Maybe TxId -> Maybe Address -> m TxList
getTransactions mCount mFrom mAddress = do
    runSdReadMLocked $ C.runConduit $
        maybe txsSource accountTxsSource mAddress (coerce mFrom)
            .| C.concatMap (mapM @WithBlock $ preview (_GMoneyTxWitnessed . twTxL))
            .| C.mapM (liftSdM . fetchBlockInfo)
            .| sinkTruncate count
  where
    count = min 100 $ fromMaybe 100 mCount

getTransactionInfo :: WitnessWorkMode ctx m => GTxId -> m GTxInfo
getTransactionInfo txId = runSdMempoolLocked $
    getTxWithBlock txId >>=
    fetchBlockInfo . fmap @WithBlock unGTxWitnessed

getPublications
    :: WitnessWorkMode ctx m
    => Maybe Int -> Maybe PublicationTxId -> Maybe Address -> m PublicationList
getPublications mCount mFrom mEducator = do
    runSdReadMLocked $ C.runConduit $
        maybe publicationsSource educatorPublicationsSource mEducator mFrom
            .| C.mapM (liftSdM . fetchBlockInfo)
            .| sinkTruncate count
  where
    count = min 100 $ fromMaybe 100 mCount

-- | As we can't distinguish between different hashes, we have to check whether an entity exists.
getHashType :: WitnessWorkMode ctx m => Text -> m HashIs
getHashType someHash = fmap (fromMaybe HashIsUnknown) . runMaybeT . asum $
    [isBlock, isAddress, isTx] <*> [someHash]
  where
    is hashType decode getMaybe =
        MaybeT . return . rightToMaybe . decode >=>
        MaybeT . getMaybe >=>
        MaybeT . return . Just . hashType

    isBlock = is
        (const HashIsBlock)
        (fromHex @HeaderHash)
        (runSdMempoolLocked . getBlockMaybe)
    isAddress = is
        (const HashIsAddress)
        addrFromText
        (\x -> runSdReadMLocked @'ChainAndMempool $ getMempoolAccountMaybe x)
    isTx = is
        (distinguishTx . unGTxWitnessed)
        fromHex
        (runSdMempoolLocked . getTxMaybe . GTxId)
    distinguishTx = \case
        GMoneyTx _ -> HashIsMoneyTx
        GPublicationTx _ -> HashIsPublicationTx
