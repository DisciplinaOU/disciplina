module Dscp.Educator.Logic.Publishing
    ( dumpPrivateBlock
    , updateMempoolWithPublications
    ) where

import Fmt (build, fmt, listF, nameF)
import Loot.Log (logInfo)
import Snowdrop.Util (OldestFirst (..))

import Dscp.Core
import Dscp.DB.SQLite
import Dscp.Educator.DB
import Dscp.Educator.Error
import Dscp.Educator.Launcher.Mode
import Dscp.Resource.Keys
import Dscp.Util
import Dscp.Witness

-- | Form and store a new private block made up from hanging private transactions.
dumpPrivateBlock :: EducatorWorkMode ctx m => m (Maybe PrivateBlockHeader)
dumpPrivateBlock = do
    ctx <- ask
    mblock <- transact $ createPrivateBlock ctx Nothing
    case mblock of
        Nothing ->
            logInfo "No private chain updates, skipping private block creation"
        Just block ->
            logInfo . fmt $
                nameF "Created new private block" (build block)
    return mblock

-- | Update mempool with lacking private blocks.
-- Heavyweight operation.
publishMissingBlocks :: EducatorWorkMode ctx m => m ()
publishMissingBlocks =
    writingSDLock "add publication to mempool" $ do
        ctx <- ask
        sk <- ourSecretKeyData @EducatorNode
        privTip <- runSdMempool $ getPrivateTipHash (skAddress sk)
        let feePolicy = fcPublication feeConfig

        OldestFirst blocks :: OldestFirst [] PrivateBlockHeader <-
            invoke (getPrivateBlocksAfterHash ctx privTip)
            >>= nothingToThrow (PrivateAndPublicChainsDiverged privTip)
        let pubTxs = createPublicationTxw feePolicy sk <$> blocks

        logInfo . fmt $ nameF "Adding publications to mempool" (listF $ toPtxId . ptwTx <$> pubTxs)
        forM_ pubTxs $ addTxToMempool . GPublicationTxWitnessed

-- | Update mempool with lacking private blocks, if needed.
-- Returns whether the update was performed.
updateMempoolWithPublications :: EducatorWorkMode ctx m => m Bool
updateMempoolWithPublications = do
    ctx <- ask
    sk <- ourSecretKeyData @EducatorNode
    publishedPrivTip <- runSdMempoolLocked $ getPrivateTipHash (skAddress sk)
    storedPrivTip <- invoke (fst <$> getLastBlockIdAndIdx ctx)

    let needUpdate = publishedPrivTip /= storedPrivTip
    when needUpdate publishMissingBlocks
    return needUpdate
