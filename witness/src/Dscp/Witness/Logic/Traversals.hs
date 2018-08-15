-- | Block/header traversals.

module Dscp.Witness.Logic.Traversals
    ( loadDownWhile
    , loadHashesDownWhile
    , loadHeadersDownWhile
    , loadBlocksDownWhile

    , getBlocksFromTo
    , getBlocksBefore

    , getTxs
    ) where

import Control.Monad.Trans.Except (throwE)
import qualified Data.List.NonEmpty as NE
import qualified Snowdrop.Core as SD

import Dscp.Core
import Dscp.Snowdrop
import Dscp.Util
import Dscp.Witness.Config
import Dscp.Witness.Logic.Getters

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

-- | Retrieves the requested amount of recent transactions
getTxs ::
       HasWitnessConfig
    => Int
    -> Maybe GTxId
    -> SdM (Either Text (OldestFirst [] GTxInBlock))
getTxs depth = \case
    Nothing -> getTipBlock >>= map (Right . OldestFirst) . loadTxs [] depth Nothing
    Just gTxId -> runExceptT $ do
        TxBlockRef{..} <- ExceptT $ maybeToRight ("Can't get block ref for tx " <> show gTxId) <$>
            SD.queryOne gTxId
        block <- ExceptT $ maybeToRight ("Can't get block " <> show tbrBlockRef) <$>
            getBlockMaybe tbrBlockRef
        txs <- lift $ loadTxs [] depth (Just $ tbrTxIdx + 1) block
        pure $ OldestFirst txs
  where
    loadTxs !res depthLeft mLimit block
        | depthLeft == 0 =
            return res
        | limit >= depthLeft =
            return $ drop (limit - depthLeft) txs ++ res
        | otherwise = runMaybeT (MaybeT (resolvePrevious block) >>= MaybeT . getBlockMaybe) >>= \case
            Nothing -> return $ txs ++ res
            Just nextBlock -> loadTxs (txs ++ res) (depthLeft - limit) Nothing nextBlock
      where
        limit = fromMaybe (length txs) mLimit
        txs = map (GTxInBlock (Just block)) . bbTxs . bBody $ block
