-- | Common logic for educator and student API's.

module Dscp.Educator.Web.Logic
    ( commonGetProofs
    , getEducatorStatus
    ) where

import Universum

import Dscp.Core
import Dscp.DB.SQL
import Dscp.Educator.DB
import Dscp.Educator.Launcher.Mode
import Dscp.Educator.Resource
import Dscp.Educator.Web.Educator.Types
import Dscp.Educator.Web.Types
import Dscp.Util.Aeson

commonGetProofs
    :: MonadEducatorWebQuery m
    => GetProvenStudentTransactionsFilters
    -> DBT 'WithinTx m [BlkProofInfo]
commonGetProofs filters = do
    rawProofs <- getProvenStudentTransactions filters
    return
        [ BlkProofInfo
          { bpiBlockHash = bHash
          , bpiMtreeSerialized = EncodeSerialised mtree
          , bpiTxs = txs
          }
        | (bHash, mtree, txs) <- rawProofs
        ]

getEducatorStatus
    :: MonadEducatorWeb ctx m
    => m EducatorInfo
getEducatorStatus = do
    sk <- ourSecretKeyData @EducatorNode
    let eiAddress = skAddress sk
    return EducatorInfo {..}
