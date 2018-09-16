-- | Common logic for educator and student API's.

module Dscp.Educator.Web.Logic
    ( commonGetProofs
    ) where

import Dscp.Core
import Dscp.DB.SQLite
import Dscp.Educator.Web.Types
import Dscp.Util.Aeson

commonGetProofs
    :: MonadEducatorWebQuery m
    => Student
    -> GetProvenStudentTransactionsFilters
    -> DBT WithinTx w m [BlkProofInfo]
commonGetProofs student filters = do
    rawProofs <- getProvenStudentTransactions student filters
    return
        [ BlkProofInfo{ bpiMtreeSerialized = CustomEncoding mtree, bpiTxs }
        | (mtree, indicedTxs) <- rawProofs
        , let bpiTxs = map snd indicedTxs
        ]
