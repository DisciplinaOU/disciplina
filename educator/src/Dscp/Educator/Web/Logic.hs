-- | Common logic for educator and student API's.

module Dscp.Educator.Web.Logic
    ( commonGetProofs
    ) where

import Dscp.DB.SQLite
import Dscp.Educator.Web.Types
import Dscp.Util.Aeson

commonGetProofs
    :: MonadEducatorWebQuery m
    => GetProvenStudentTransactionsFilters
    -> DBT 'WithinTx w m [BlkProofInfo]
commonGetProofs filters = do
    rawProofs <- getProvenStudentTransactions filters
    return
        [ BlkProofInfo{ bpiMtreeSerialized = CustomEncoding mtree, bpiTxs }
        | (mtree, indicedTxs) <- rawProofs
        , let bpiTxs = map snd indicedTxs
        ]
