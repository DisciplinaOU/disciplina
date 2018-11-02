-- | Block/header traversals.

module Dscp.Witness.Logic.Traversals
    ( loadDownWhile
    , loadHashesDownWhile
    , loadHeadersDownWhile
    , loadBlocksDownWhile

    , getBlocksFromTo
    , getBlocksFrom
    , getBlocksBefore

    , txsSource
    , chainTxsSource
    , accountTxsSource
    , chainPublicationsSource
    , publicationsSource
    , educatorPublicationsSource
    ) where

import Control.Monad.Trans.Except (throwE)
import Data.Coerce (coerce)
import Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.List.NonEmpty as NE
import Fmt ((+|), (|+))
import qualified Snowdrop.Core as SD
import qualified Snowdrop.Util as SD

import Dscp.Core
import Dscp.Snowdrop
import Dscp.Util
import Dscp.Witness.Config
import Dscp.Witness.Launcher.Context
import Dscp.Witness.Logic.Exceptions
import Dscp.Witness.Logic.Getters
import Dscp.Witness.Mempool
import Dscp.Witness.SDLock

----------------------------------------------------------------------------
-- One-direction traversals
----------------------------------------------------------------------------

foldlGeneric
    :: forall a b m r .
    ( Monad m, HasHeaderHash a )
    => (HeaderHash -> m (Maybe HeaderHash)) -- ^ Get next element
    -> (HeaderHash -> m (Maybe b))          -- ^ For each header we get b(lund)
    -> a                                    -- ^ We start iterating from it
    -> (b -> Int -> Bool)                   -- ^ While condition on b and depth
    -> (r -> b -> m r)                      -- ^ Conversion function
    -> r                                    -- ^ Starting value
    -> m r
foldlGeneric getNext getData start condition accM initial =
    loadUpWhileDo (headerHash start) 0 initial
  where
    loadUpWhileDo :: HeaderHash -> Int -> r -> m r
    loadUpWhileDo curH height !res = getData curH >>= \case
        Nothing -> pure res
        Just someData -> do
            mbNextLink <- getNext curH
            if | not (condition someData height) -> pure res
               | Just nextLink <- mbNextLink -> do
                     newRes <- accM res someData
                     loadUpWhileDo nextLink (succ height) newRes
               | otherwise -> accM res someData

-- Loads something from old to new. foldlUpWhileM for (OldestFirst []).
loadDownWhile
    :: forall a b . (HasHeaderHash a, HasWitnessConfig)
    => (HeaderHash -> SdM (Maybe b))
    -> a
    -> (b -> Int -> Bool)
    -> SdM (OldestFirst NonEmpty b)
loadDownWhile morph start condition = OldestFirst . NE.fromList <$>
    foldlGeneric
        resolvePrevious
        morph
        start
        condition
        (\l e -> pure (e : l))
        []

-- | Return hashes loaded up. Basically a forward links traversal.
loadHashesDownWhile
    :: forall a. (HasHeaderHash a, HasWitnessConfig)
    => a
    -> (HeaderHash -> Int -> Bool)
    -> SdM (OldestFirst NonEmpty HeaderHash)
loadHashesDownWhile = loadDownWhile (pure . Just)

-- | Return hashes loaded up. Basically a forward links traversal.
loadHeadersDownWhile
    :: forall a. (HasHeaderHash a, HasWitnessConfig)
    => a
    -> (Header -> Int -> Bool)
    -> SdM (OldestFirst NonEmpty Header)
loadHeadersDownWhile = loadDownWhile getHeaderMaybe

-- | Return hashes loaded up. Basically a forward links traversal.
loadBlocksDownWhile
    :: forall a. (HasHeaderHash a, HasWitnessConfig)
    => a
    -> (Block -> Int -> Bool)
    -> SdM (OldestFirst NonEmpty Block)
loadBlocksDownWhile = loadDownWhile getBlockMaybe

----------------------------------------------------------------------------
-- From/to
----------------------------------------------------------------------------

-- | Retrieves blocks from older to newer.
getBlocksFromTo ::
       (HasWitnessConfig, HasHeaderHash a)
    => a -- ^ Older element
    -> a -- ^ Newer element
    -> SdM (Either Text (OldestFirst NonEmpty Block))
getBlocksFromTo (headerHash -> olderH) (headerHash -> newerH) = runExceptT $ do
    olderBlock <-
        ExceptT $ maybeToRight ("Can't get older block: " <> show olderH) <$>
        getBlockMaybe olderH
    newerBlock <-
        ExceptT $ maybeToRight ("Can't get newer block: " <> show newerH) <$>
        getBlockMaybe newerH

    let dOlder = hDifficulty $ bHeader olderBlock
    let dNewer = hDifficulty $ bHeader newerBlock
    when (dNewer < dOlder) $
        throwE "Headers passed in wrong order: newer is < than older"

    if olderBlock == newerBlock
    then pure $ OldestFirst $ olderBlock :| []
    else do
        -- Exactly this number of blocks we want to retrieve
        let depthDiff = fromIntegral $ unDifficulty $ dNewer - dOlder + 1
        let loadCond _block depth = depth < depthDiff
        blocks <- lift $ loadBlocksDownWhile newerH loadCond

        let retrievedOldest = NE.head (unOldestFirst blocks)

        -- sanity checks, remove them later
        when (NE.length (unOldestFirst blocks) < depthDiff &&
              retrievedOldest /= genesisBlock) $
            error "getBlocksFromTo: retrieved less than expected"

        when (NE.length (unOldestFirst blocks) > depthDiff) $
            error "getBlocksFromTo: retrieved too many"

        unless (retrievedOldest `elem` [olderBlock,genesisBlock]) $
            error "getBlocksFromTo: unexpected oldest block"

        pure blocks

-- | Retrieves the requested amount of blocks skipping `skip` blocks from tip.
getBlocksFrom ::
       HasWitnessConfig
    => Word64
    -> Int
    -> SdM (Either Text (OldestFirst NonEmpty Block))
getBlocksFrom skip depthDiff = runExceptT $ do
    tipIdx <- unDifficulty . hDifficulty <$> lift getTipHeader

    newerH <- ExceptT $ maybeToRight "Can't skip so many blocks" <$> SD.queryOne (Difficulty $ tipIdx - skip)

    let loadCond _block depth = depth < depthDiff
    blocks <- lift $ loadBlocksDownWhile newerH loadCond

    let retrievedOldest = NE.head (unOldestFirst blocks)

    -- sanity checks, remove them later
    when (NE.length (unOldestFirst blocks) < depthDiff &&
          retrievedOldest /= genesisBlock) $
        error "getBlocksFrom: retrieved less than expected"

    when (NE.length (unOldestFirst blocks) > depthDiff) $
        error "getBlocksFrom: retrieved too many"

    pure blocks

-- | Retrieves the requested amount of blocks starting with the given block down.
getBlocksBefore ::
       (HasWitnessConfig, HasHeaderHash a)
    => Int
    -> a
    -> SdM (Either Text (OldestFirst NonEmpty Block))
getBlocksBefore depthDiff (headerHash -> newerH) = runExceptT $ do
    void $ ExceptT $ maybeToRight ("Can't get newer block: " <> show newerH) <$>
        getBlockMaybe newerH

    let loadCond _block depth = depth < depthDiff
    blocks <- lift $ loadBlocksDownWhile newerH loadCond

    let retrievedOldest = NE.head (unOldestFirst blocks)

    -- sanity checks, remove them later
    when (NE.length (unOldestFirst blocks) < depthDiff &&
          retrievedOldest /= genesisBlock) $
        error "getBlocksFromTo: retrieved less than expected"

    when (NE.length (unOldestFirst blocks) > depthDiff) $
        error "getBlocksFromTo: retrieved too many"

    pure blocks

-- | Retrieves transactions starting with the given one down the chain.
-- If no transaction is provided, transactions are retrieved starting from the tip.
chainTxsSource
    :: (WitnessWorkMode ctx m, KnownSdReadMode mode, WithinReadSDLock)
    => Maybe GTxId -> C.ConduitT () (WithBlock GTxWitnessed) (SdReadM mode m) ()
chainTxsSource = \case
    Nothing -> lift2xSdM getTipBlock >>= loadTxs Nothing
    Just gTxId -> do
        (block, txIdx) <- lift2xSdM $ do
            TxBlockRef{..} <- (TxBlockRefId gTxId) `assertExists` LETxAbsent ("Can't get block ref for tx " <> show gTxId)
            block <- nothingToLocalError (LEMalformed $ "Can't get block " <> show tbrBlockRef)
                =<< getBlockMaybe tbrBlockRef
            return (block, tbrTxIdx)
        loadTxs (Just txIdx) block
  where
    loadTxs mIdx block = do
        -- last txs in the block go first
        C.yieldMany (reverse $ maybe id (take . succ) mIdx txs)

        nextBlock <- lift2xSdM . runMaybeT $
            MaybeT (resolvePrevious block) >>= MaybeT . getBlockMaybe
        whenJust nextBlock (loadTxs Nothing)
      where
        blockTxs = bbTxs . bBody $ block
        txs = WithBlock (Just block) <$> blockTxs

-- | Get a list of all transactions for a given account.
accountTxsSource
    :: (WitnessWorkMode ctx m, KnownSdReadMode mode, WithinReadSDLock)
    => Address
    -> Maybe GTxId
    -> C.ConduitT () (WithBlock GTxWitnessed) (SdReadM mode m) ()
accountTxsSource address mStart =
    loadTxs .| C.mapM (liftSdM . getTxWithBlock)
  where
    loadTxs = case mStart of
        Nothing ->
            lift2xSdM (SD.queryOne (TxsOf address)) >>=
            loadNextTx . map unLastTx
        Just start ->
            loadNextTx (Just start)
    loadNextTx = \case
        Nothing -> pass
        Just gTxId -> do
            C.yield gTxId
            lift2xSdM (SD.queryOne (TxHead address gTxId)) >>=
                loadNextTx . map unTxNext

-- | Get all mempool transactions.
mempoolTxsSource
    :: (WitnessWorkMode ctx m, WithinReadSDLock)
    => C.ConduitT () GTxWitnessed (SdReadM 'ChainAndMempool m) ()
mempoolTxsSource = lift (lift $ readTxsMempool) >>= C.yieldMany

-- | Get all transactions, from both chain and mempool, starting from the given one.
txsSource
    :: (WitnessWorkMode ctx m, WithinReadSDLock)
    => Maybe GTxId
    -> C.ConduitT () (WithBlock GTxWitnessed) (SdReadM 'ChainAndMempool m) ()
txsSource Nothing = do
    mempoolTxsSource .| C.map (WithBlock Nothing)
    chainTxsSource Nothing
txsSource (Just start) = do
    lift2xSdM (SD.queryOne $ TxBlockRefId start) >>= \case
        Just TxBlockRef{} ->
            chainTxsSource (Just start)
        Nothing -> do
            mempoolTxsSource
                .| C.dropWhile ((/= start) . toGTxwId)
                .| C.map (WithBlock Nothing)
                .| ensureHasAtLeastOneTx
            chainTxsSource Nothing
  where
    ensureHasAtLeastOneTx =
        C.await >>= \case
            Nothing -> lift2xSdM $ SD.throwLocalError $
                       LETxAbsent $ "Transaction not found: " <> show start
            Just tx -> C.leftover tx

-- | Retrieves publication transaction starting with the given one down the chain.
-- If no transaction is provided, blocks are retrieved starting from the tip.
chainPublicationsSource
    :: (WitnessWorkMode ctx m, KnownSdReadMode mode, WithinReadSDLock)
    => Maybe PublicationTxId
    -> C.ConduitT () (WithBlock PublicationTx) (SdReadM mode m) ()
chainPublicationsSource mStart' = do
    let mStart = coerce @(Maybe PublicationTxId) @(Maybe GTxId) mStart'
    chainTxsSource mStart
        .| C.concatMap (mapM @WithBlock $ preview (_GPublicationTxWitnessed . ptwTxL))

-- | Retrieves publication transaction, both from chain and mempool, starting from the
-- given one.
publicationsSource
    :: (WitnessWorkMode ctx m, WithinReadSDLock)
    => Maybe PublicationTxId
    -> C.ConduitT () (WithBlock PublicationTx) (SdReadM 'ChainAndMempool m) ()
publicationsSource mStart' = do
    let mStart = coerce @(Maybe PublicationTxId) @(Maybe GTxId) mStart'
    txsSource mStart
        .| C.concatMap (mapM @WithBlock $ preview (_GPublicationTxWitnessed . ptwTxL))

-- | Retrieves private blocks of the given educator starting with the given one
-- down the chain.
-- If no transaction is provided, blocks are retrieved starting from the most
-- recent one.
educatorPublicationsSource
    :: (WitnessWorkMode ctx m, KnownSdReadMode mode, WithinReadSDLock)
    => Address
    -> Maybe PublicationTxId
    -> C.ConduitT () (WithBlock PublicationTx) (SdReadM mode m) ()
educatorPublicationsSource educator = \case
    Nothing -> do
        mheader <- lift2xSdM $ SD.queryOne (PublicationsOf educator)
        whenJust mheader $ \(LastPublication header) -> do
            ptxId <- lift2xSdM $ header `assertExists` noPrivHeader header
            loadChain ptxId
    Just ptxId -> loadChain ptxId
  where
    loadChain ptxId = do
        ptx <-
            lift2xSdM $ fmap pdTx $ ptxId `assertExists` noPubTxId ptxId
        mblock <- lift2xSdM $ do
            mBlockHash <- SD.queryOne (TxBlockRefId $ coerce ptxId)
            mapM (getBlock . tbrBlockRef) mBlockHash
        C.yield $ WithBlock mblock ptx

        let prevHash = _pbhPrevBlock (ptHeader ptx)
        unless (prevHash == genesisHeaderHash) $
            lift2xSdM (prevHash `assertExists` noPrivHeader prevHash)
            >>= loadChain

    noPrivHeader (h :: PrivateHeaderHash) =
        LEPrivateBlockAbsent $ "Publication for such private header hash not \
                               \found: " +| h |+ ""
    noPubTxId (ptxId :: PublicationTxId) =
        LEMalformed $ "No publication transaction found: " +| ptxId |+ ""
