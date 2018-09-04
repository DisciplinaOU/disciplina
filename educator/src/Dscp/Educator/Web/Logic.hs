-- | Common logic for educator and student API's.

module Dscp.Educator.Web.Logic
    ( commonGetProofs
    ) where

import Dscp.Core
import Dscp.DB.SQLite.Queries
import Dscp.Educator.Web.Queries
import Dscp.Educator.Web.Types
import Dscp.Util.Aeson

commonGetProofs
    :: MonadEducatorQuery m
    => Student
    -> GetProvenStudentTransactionsFilters
    -> m [BlkProofInfo]
commonGetProofs student filters = do
    rawProofs <- getProvenStudentTransactions student filters
    return
        [ BlkProofInfo{ bpiMtreeSerialized = CustomEncoding mtree, bpiTxs }
        | (mtree, indicedTxs) <- rawProofs
        , let bpiTxs = map snd indicedTxs
        ]
