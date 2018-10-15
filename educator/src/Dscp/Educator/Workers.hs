{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists  #-}

module Dscp.Educator.Workers
       ( educatorWorkers
       ) where

import Fmt ((+||), (||+))
import Loot.Config (option, sub)
import Loot.Log (logWarning)
import Time (ms, sec, toUnit)

import Dscp.Core
import Dscp.Educator.Config
import Dscp.Educator.Launcher.Mode
import Dscp.Educator.Logic
import Dscp.Network
import Dscp.Util.Timing

educatorWorkers
    :: EducatorWorkMode ctx m
    => [Worker m]
educatorWorkers =
    [ privateBlockCreatorWorker
    , publicationTxSubmitter
    ]

----------------------------------------------------------------------------
-- Private blocks publishing
----------------------------------------------------------------------------

-- | Periodically take hanging private transactions and form a new private block.
privateBlockCreatorWorker :: EducatorWorkMode ctx m => Worker m
privateBlockCreatorWorker =
    Worker "privateBlockCreatorWorker" [] [] $ \_ -> bootstrap >> work
  where
    period = educatorConfig ^. sub #educator . sub #publishing . option #period
    slotDuration =
        toUnit . ms $ fromIntegral $ unSlotDuration $ giveL @WitnessConfig

    bootstrap =
        unless (period > slotDuration) $
            logWarning $ "Private block publishing period is not greater than \
                         \witness slot duration ("
                         +|| period ||+ " <= " +|| slotDuration ||+ ")"

    work =
        foreverAlive "Private block publisher" period $
        notFasterThan period $
            void dumpPrivateBlock

-- | Publish all hanging private blocks to public chain.
publicationTxSubmitter :: forall m ctx. EducatorWorkMode ctx m => Worker m
publicationTxSubmitter =
    Worker "publicationTxSubmitter" [] [] $ \_ ->
        foreverAlive "Tx publisher" (sec 10) $
        notFasterThan (sec 1) $
            void updateMempoolWithPublications
