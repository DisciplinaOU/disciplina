{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Student API handlers

module Dscp.Witness.Web.Handlers
       ( witnessServantHandlers
       ) where

import Servant
import Servant.Generic (AsServerT, toServant)

import Dscp.Core
import Dscp.Witness.Launcher.Context
import Dscp.Witness.Web.API
import Dscp.Witness.Web.Logic

witnessServantHandlers
    :: forall m ctx. WitnessWorkMode ctx m
    => ServerT WitnessAPI m
witnessServantHandlers =
    toServant @(WitnessEndpoints (AsServerT m)) WitnessEndpoints
    { wPing = pass
    , wSubmitTx = \tw ->
        submitUserTx tw $> toTxId (twTx tw)
    , wSubmitPublication = \ptx ->
        submitUserPublicationTx ptx $> toPtxId (ptwTx ptx)
    , wSubmitTxAsync = \tw ->
        submitUserTxAsync tw $> toTxId (twTx tw)
    , wGetBlocks = getBlocks
    , wGetBlock = getBlockInfo
    , wGetAccount = getAccountInfo
    , wGetTransactions = getTransactions
    , wGetTransaction = getTransactionInfo
    , wGetPublications = getPublications
    , wGetHashType = getHashType
    , wCheckFairCV = checkFairCV
    , wCheckFairCVPDF = checkFairCVPDF
    }
