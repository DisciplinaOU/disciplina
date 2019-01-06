-- | Common logic for educator and student API's.

module Dscp.Educator.Web.Logic
    ( commonGetProofs
    , getEducatorStatus
    ) where

import Data.Default (def)
import Servant.Util (FilteringSpecOf)

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
    => FilteringSpecOf BlkProofInfo
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
    let address = skAddress sk

    accounts <- readingSDLock $ runSdDual $
        fromMaybe def <$> getAccountMaybe address
    return EducatorInfo
        { eiAddress = address
        , eiBalances = Coin . fromIntegral . aBalance <$> accounts
        }
