module Dscp.Educator.Logic.Publishing
    ( dumpPrivateBlock
    , updateMempoolWithPublications
    ) where

import Universum

import Fmt (build, fmt, nameF)
import Loot.Log (logInfo)

import Dscp.Core
import Dscp.DB.SQL
import Dscp.Educator.DB
-- import Dscp.Educator.Error
import Dscp.Educator.Launcher.Mode

-- | Form and store a new private block made up from hanging private transactions.
dumpPrivateBlock :: EducatorWorkMode ctx m => m (Maybe PrivateBlockHeader)
dumpPrivateBlock = do
    mblock <- transact $ dumpNonChainedTransactions
    case mblock of
        Nothing ->
            logInfo "No private chain updates, skipping private block creation"
        Just (_, block) ->
            logInfo . fmt $
                nameF "Created new private block" (build block)
    return $ fmap snd mblock

-- | Update mempool with lacking private blocks.
-- Heavyweight operation.
-- UPD: removed witness-based logic here

-- publishMissingBlocks :: EducatorWorkMode ctx m => m ()
-- publishMissingBlocks = pass
    -- monitors <- writingSDLock "add publication to mempool" $ do
    --     sk <- ourSecretKeyData @EducatorNode
    --     privTip <- runSdMempool $ getPrivateTipHash (skAddress sk)
    --     let feePolicy = feeConfig ^. option #publication

    --     OldestFirst blocks <-
    --         invoke (getPrivateBlocksAfterHash privTip)
    --         >>= nothingToThrow (PrivateAndPublicChainsDiverged privTip)
    --     let pubTxs = createPublicationTxw feePolicy sk <$> blocks

    --     logInfo . fmt $ nameF "Adding publications to mempool" (listF $ toPtxId . ptwTx <$> pubTxs)
    --     forM pubTxs $ relayTx . GPublicationTxWitnessed

    -- forM_ monitors $ wait @"tx in mempool"

-- | Update mempool with lacking private blocks, if needed.
-- Returns whether the update was performed.
-- UPD: removed witness-based logic here
updateMempoolWithPublications :: Monad m => m Bool
updateMempoolWithPublications = return False
    -- sk <- ourSecretKeyData @EducatorNode
    -- publishedPrivTip <- runSdMempoolLocked $ getPrivateTipHash (skAddress sk)
    -- (storedPrivTip, _) <- invoke getLastBlockIdAndIdx

    -- let needUpdate = publishedPrivTip /= storedPrivTip
    -- when needUpdate publishMissingBlocks
    -- return needUpdate
