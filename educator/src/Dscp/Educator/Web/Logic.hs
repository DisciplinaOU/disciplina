-- | Common logic for educator and student API's.

module Dscp.Educator.Web.Logic
    ( commonGetProofs
    , getEducatorStatus
    ) where

import Data.Default (def)
import Dscp.Core
import Dscp.DB.SQL
import Dscp.Educator.DB
import Dscp.Educator.Launcher.Mode
import Dscp.Educator.Web.Educator.Types
import Dscp.Educator.Web.Types
import Dscp.Resource.Keys
import Dscp.Snowdrop.Types
import Dscp.Util.Aeson
import Dscp.Witness.Logic.Getters
import Dscp.Witness.SDLock

commonGetProofs
    :: MonadEducatorWebQuery m
    => GetProvenStudentTransactionsFilters
    -> DBT 'WithinTx m [BlkProofInfo]
commonGetProofs filters =
    map toBlkProofInfo <$> getProvenStudentTransactions filters
  where
    toBlkProofInfo (bHash, mtree, txs) =
        BlkProofInfo
        { bpiBlockHash = bHash
        , bpiMtreeSerialized = EncodeSerialised mtree
        , bpiTxs = txs
        }

getEducatorStatus
    :: MonadEducatorWeb ctx m
    => m EducatorInfo
getEducatorStatus = do
    sk <- ourSecretKeyData @EducatorNode
    let address = skAddress sk

    accounts <- readingSDLock $ runSdDual $
        fromMaybe def <$> getAccountMaybe address
    return EducatorInfo
        { eiAddress = address
        , eiBalances = Coin . fromIntegral . aBalance <$> accounts
        }
