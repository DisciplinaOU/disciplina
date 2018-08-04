-- | Block/header traversals.

module Dscp.Witness.Logic.Traversals
    ( loadDownWhile
    , loadHashesDownWhile
    , loadHeadersDownWhile
    , loadBlocksDownWhile
    ) where

import Dscp.Core
import Dscp.Snowdrop
import Dscp.Util
import Dscp.Witness.Config
import Dscp.Witness.Logic.Getters

foldlGeneric
    :: forall a b m r .
    ( Monad m, HasHeaderHash a )
    => (HeaderHash -> m (Maybe HeaderHash)) -- ^ Get next element
    -> (HeaderHash -> m (Maybe b))          -- ^ For each header we get b(lund)
    -> a                                    -- ^ We start iterating from it
    -> (b -> Int -> Bool)                   -- ^ Condition on b and depth
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
    -> SdM (OldestFirst [] b)
loadDownWhile morph start condition = OldestFirst <$>
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
    -> SdM (OldestFirst [] HeaderHash)
loadHashesDownWhile = loadDownWhile (pure . Just)

-- | Return hashes loaded up. Basically a forward links traversal.
loadHeadersDownWhile
    :: forall a. (HasHeaderHash a, HasWitnessConfig)
    => a
    -> (Header -> Int -> Bool)
    -> SdM (OldestFirst [] Header)
loadHeadersDownWhile = loadDownWhile getHeaderMaybe

-- | Return hashes loaded up. Basically a forward links traversal.
loadBlocksDownWhile
    :: forall a. (HasHeaderHash a, HasWitnessConfig)
    => a
    -> (Block -> Int -> Bool)
    -> SdM (OldestFirst [] Block)
loadBlocksDownWhile = loadDownWhile getBlockMaybe
