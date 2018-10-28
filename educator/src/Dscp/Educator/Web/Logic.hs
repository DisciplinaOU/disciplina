-- | Common logic for educator and student API's.

module Dscp.Educator.Web.Logic
    ( commonGetProofs
    , getEducatorStatus
    ) where

import Data.Default (def)
import Dscp.Core
import Dscp.DB.SQLite
import Dscp.Educator.Launcher.Mode
import Dscp.Educator.Web.Educator.Types
import Dscp.Educator.Web.Types
import Dscp.Resource.Keys
import Dscp.Snowdrop.Mode
import Dscp.Snowdrop.Types
import Dscp.Util.Aeson
import Dscp.Witness.Logic.Getters
import Dscp.Witness.SDLock
import Dscp.Witness.Web.Types

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

getEducatorStatus
    :: MonadEducatorWeb ctx m
    => m EducatorInfo
getEducatorStatus = do
    sk <- ourSecretKeyData @EducatorNode
    let address = skAddress sk

    -- TODO [DSCP-367]: to replace with one-liner
    accounts <- readingSDLock $ do
        blockAccount <- runSdReadM $ getAccountMaybe address
        poolAccount  <- runSdReadM $ getMempoolAccountMaybe address
        return $ fromMaybe def <$>
                 BlocksOrMempool{ bmConfirmed = blockAccount, bmTotal = poolAccount }

    return EducatorInfo
        { eiAddress = address
        , eiBalances = Coin . fromIntegral . aBalance <$> accounts
        }
